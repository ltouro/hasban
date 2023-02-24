{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use if" #-}

module LoanCalculator
  ( LoanQuery(..)
  , Frequency(..)
  , Commission
  , CommissionType
  , AmortizationType
  , queryLoan
  , asLiquidValueLoan
  , LoanQueryResult
  ) where

import           Data.Aeson
import           Data.Time.Calendar     (addDays, addGregorianMonthsClip,
                                         addGregorianYearsClip, diffDays,
                                         fromGregorian, toGregorian)
import           GHC.Generics
import           Text.PrettyPrint.Boxes (hsep, left, render, text, vsep)
import           Text.Printf            (printf)

data LoanQuery = LoanQuery
  { amQueryAmount       :: Double
  , amQueryRate         :: Double
  , amQueryTerms        :: Int
  , amQueryFrequency    :: Frequency
  , amQueryStartDate    :: Date
  , amQueryAbsInterest  :: Int
  , amQueryAbsPrincipal :: Int
  , amQueryCommissions  :: [Commission]
  , amQueryAmortizationType  :: AmortizationType
  }
  deriving (Show, Generic)
instance FromJSON LoanQuery
instance ToJSON LoanQuery

data AmortizationTableItem = AmortizationTableItem
  { amTableDate      :: Date
  , amTablePaymentNo :: Int
  , amTableInterest  :: Double
  , amTablePrincipal :: Double
  , amTableRemaining :: Double
  , amTableTotal     :: Double
  , amTableTax       :: Double
  }
  deriving (Eq, Show, Read, Generic)
instance FromJSON AmortizationTableItem
instance ToJSON AmortizationTableItem

newtype AmortizationTable = AmortizationTable
  { amTable :: [AmortizationTableItem]
  } deriving (Eq, Read, Generic)
instance FromJSON AmortizationTable
instance ToJSON AmortizationTable

data LoanQueryResult = LoanQueryResult
  { loanResultTable       :: AmortizationTable
  , loanResultLiquidValue :: Double
  , loanResultTax         :: Double
  , loanResultPrincipal   :: Double
  , loanResultInterest    :: Double
  , loanResultTotal       :: Double
  , loanResultCommissions :: Double
  }
  deriving (Eq, Read, Generic)
instance FromJSON LoanQueryResult
instance ToJSON LoanQueryResult

data Commission = Commission
  { commissionType  :: CommissionType
  , commissionValue :: Double
  }
  deriving (Show, Generic)
instance FromJSON Commission
instance ToJSON Commission

data CommissionType = Absolute | Percentage deriving (Show, Generic)
instance FromJSON CommissionType
instance ToJSON CommissionType

data AmortizationType =
    PRICE |
    SAC
  deriving (Enum, Show, Generic)
instance FromJSON AmortizationType
instance ToJSON AmortizationType

data Frequency =
  Monthly |
  Biweekly |
  Weekly |
  Daily |
  Yearly
  deriving (Enum, Show, Generic)
instance FromJSON Frequency
instance ToJSON Frequency

-- FV = PV * CR
futureValue :: Double -> Double -> Int -> Double
futureValue pv r n = pv * compositeRate r n

-- CR = (1+ r)^n
compositeRate :: Double -> Int -> Double
compositeRate r n = (1 + r) ^ n

-- This function calculates the variable monthly payment for an SAC amortization schedule
-- given the loan amount, interest rate, and loan term
calculatePaymentSac :: Double -> Double -> Int -> Double -> Double
calculatePaymentSac loanAmount interestRate loanTerm currentPrincipal =
  let amortization = loanAmount / fromIntegral loanTerm
  in  amortization + interestRate * currentPrincipal

-- This function calculates the fixed monthly payment for an PRICE amortization schedule
-- given the loan amount, interest rate, and loan term
calculatePaymentPrice :: Double -> Double -> Int -> Double
calculatePaymentPrice loanAmount interestRate loanTerm =
  let paymentNo = fromIntegral loanTerm
  in  loanAmount
        * futureValue interestRate interestRate paymentNo
        / (compositeRate interestRate paymentNo - 1)

-- Sums the amTableTax of every AmortizationTableItem in table
sumTax :: AmortizationTable -> Double
sumTax table = foldr (\p acc -> acc + amTableTax p) 0 (amTable table)

-- Gets the initial remaninig amount and subtract taxes
liquidValue :: LoanQuery -> AmortizationTable -> Double
liquidValue loan table =
  amTableRemaining (head (amTable table))
    - sumTax table
    - totalCommissionValue loan

queryLoan :: LoanQuery -> LoanQueryResult
queryLoan loan =
  let comm = totalCommissionValue loan
      accum field table' = foldr (\p acc -> acc + field p) 0 (amTable table')
      table = generateAmortizationTable loan
  in  LoanQueryResult
        { loanResultTable       = table
        , loanResultLiquidValue = liquidValue loan table
        , loanResultTax         = accum amTableTax table
        , loanResultPrincipal   = amTableRemaining (head (amTable table))
        , loanResultInterest    = accum amTableInterest table
        , loanResultTotal       = accum amTableTotal table
        , loanResultCommissions = comm
        }

{- $>
  let query = asLiquidValueLoan LoanQuery {
    amQueryAmount = 31500,
    amQueryRate = 0.047,
    amQueryTerms = 6,
    amQueryFrequency = Monthly,
    amQueryStartDate = (2022, 02, 28),
    amQueryAbsInterest = 0,
    amQueryAbsPrincipal = 2,
    amQueryCommissions = [Commission Percentage 2.0, Commission Percentage 2.5]
  }
<$ -}

-- $> putStr (show (queryLoan query))

-- This function generates an amortization table, given a LoanQuery
generateAmortizationTable :: LoanQuery -> AmortizationTable
generateAmortizationTable loan =
  let
    -- This functions gets the payment for a given payment number in the schedule    
    pmt paymentNo balance = case paymentNo > amQueryAbsPrincipal loan of
      -- There is a payment of principal and interest
      True -> case amQueryAmortizationType loan  of
        SAC -> calculatePaymentSac startValue (amQueryRate loan) numberOfPrincipalPayments balance
        PRICE -> calculatePaymentPrice startValue (amQueryRate loan) numberOfPrincipalPayments
      False -> case paymentNo > amQueryAbsInterest loan of
        -- There is a payment of interest
        True  -> currentInterest balance
        -- There is no payment
        False -> 0

    numberOfPrincipalPayments = amQueryTerms loan - amQueryAbsPrincipal loan

    -- The interest for given paymentNo
    currentInterest balance = balance * amQueryRate loan


    -- The value when is due the first payment of principal
    startValue = futureValue (amQueryAmount loan)
                             (amQueryRate loan)
                             (amQueryAbsInterest loan)

    -- The amount that was subtracted from the principal after this paymentNo (can be negative)
    amortization :: Int -> Double -> Double
    amortization paymentNo balance = pmt paymentNo balance- currentInterest balance

    -- Taxes for a given payment
    tax paymentNo balance = case amortization paymentNo balance > 0 of
      True  -> brazilianIofTax 0.0038 0.000082 paymentNo balance
      False -> 0

    -- BR IOF calculation dynamics for a given payment
    brazilianIofTax baseTax dailyTax paymentNo balance =
      let startDate  = amQueryStartDate loan
          dueDate    = addTerm startDate (amQueryFrequency loan) paymentNo
          am         = amortization paymentNo balance
          days       = fromIntegral (numberOfDays startDate dueDate)
          cappedDays = min 365 days
          noPayments = fromIntegral numberOfPrincipalPayments
      in  ((baseTax * amQueryAmount loan) / noPayments)
            + (dailyTax * cappedDays * am)

    initial = AmortizationTableItem { amTableDate      = amQueryStartDate loan
                                    , amTablePaymentNo = 0
                                    , amTableInterest  = 0
                                    , amTablePrincipal = 0
                                    , amTableRemaining = amQueryAmount loan
                                    , amTableTotal     = 0
                                    , amTableTax       = 0
                                    }

    itemForPaymentNo paymentNo prevItem =
      let
        balance = amTableRemaining prevItem
        interest = currentInterest balance
        payment = pmt paymentNo balance
        amort = amortization paymentNo balance
      in
      AmortizationTableItem
      { amTableDate      = addTerm (amQueryStartDate loan)
                                   (amQueryFrequency loan)
                                   paymentNo
      , amTablePaymentNo = paymentNo
      , amTableInterest  = interest
      , amTablePrincipal = max amort 0
      , amTableRemaining = balance + interest - payment
      , amTableTotal     = payment
      , amTableTax       = tax paymentNo balance
      }

    paymentItems prevItem paymentNo = itemForPaymentNo paymentNo prevItem

  in
    AmortizationTable
      {
        amTable = scanl paymentItems initial [1 .. amQueryTerms loan]
      }

-- Calculate the total value of the given list of commissions
totalCommissionValue :: LoanQuery -> Double
totalCommissionValue loan =
  let commissions = amQueryCommissions loan
      absoluteValue (Commission Absolute   value) = value
      absoluteValue (Commission Percentage _    ) = 0.0
      percentageValue (Commission Absolute   _    ) = 0.0
      percentageValue (Commission Percentage value) = value
      absTotal  = sum $ map absoluteValue commissions
      percTotal = sum $ map percentageValue commissions
  in  absTotal + ((percTotal / 100) * amQueryAmount loan)


-- Convert a LoanQuery into another where the liquidValue of the later is equal to the amount of the first
asLiquidValueLoan :: LoanQuery -> LoanQuery
asLiquidValueLoan loan =
  let query = findAmountForLiquidAmount loan
      res   = goalSeek query 0.001 (amQueryAmount loan) 250
  in  LoanQuery { amQueryAmount       = res
                , amQueryRate         = amQueryRate loan
                , amQueryTerms        = amQueryTerms loan
                , amQueryFrequency    = amQueryFrequency loan
                , amQueryStartDate    = amQueryStartDate loan
                , amQueryAbsInterest  = amQueryAbsInterest loan
                , amQueryAbsPrincipal = amQueryAbsPrincipal loan
                , amQueryCommissions  = amQueryCommissions loan
                , amQueryAmortizationType = amQueryAmortizationType loan
                }

type AccuracyLevel = Double
type Guess = Double
type MaxIterations = Int

-- finds a value trough guessing based on func results
goalSeek
  :: (Double -> Double) -> AccuracyLevel -> Guess -> MaxIterations -> Double
goalSeek func accuracyLevel guess maxIterations =
  let delta     = 0.00001
      newGuess  = guess + delta
      result1   = func guess
      result2   = func newGuess
      nextGuess = guess - (result1 * (newGuess - guess) / (result2 - result1))
  in  if abs result1 > accuracyLevel && maxIterations > 0
        then goalSeek func accuracyLevel nextGuess (maxIterations - 1)
        else guess

findAmountForLiquidAmount :: LoanQuery -> (Double -> Double)
findAmountForLiquidAmount loan = \n ->
  let testLoan = LoanQuery { amQueryAmount       = n
                           , amQueryRate         = amQueryRate loan
                           , amQueryTerms        = amQueryTerms loan
                           , amQueryFrequency    = amQueryFrequency loan
                           , amQueryStartDate    = amQueryStartDate loan
                           , amQueryAbsInterest  = amQueryAbsInterest loan
                           , amQueryAbsPrincipal = amQueryAbsPrincipal loan
                           , amQueryCommissions  = amQueryCommissions loan
                           , amQueryAmortizationType = amQueryAmortizationType loan
                           }
      val = liquidValue testLoan (loanResultTable $ queryLoan testLoan)
  in  amQueryAmount loan - val

-- return the date added of given number of periods of certain frequency
addTerm :: Date -> Frequency -> Int -> Date
addTerm (year, month, day) paymentFrequency numPeriods =
  let current = fromGregorian year month day
      next    = case paymentFrequency of
        Monthly  -> addGregorianMonthsClip (fromIntegral numPeriods) current
        Yearly   -> addGregorianYearsClip (fromIntegral numPeriods) current
        Biweekly -> addDays (fromIntegral numPeriods * 14) current
        Weekly   -> addDays (fromIntegral numPeriods * 7) current
        Daily    -> addDays (fromIntegral numPeriods) current
  in  toGregorian next

numberOfDays :: Date -> Date -> Integer
numberOfDays (year1, month1, day1) (year2, month2, day2) =
  let date1 = fromGregorian year1 month1 day1
      date2 = fromGregorian year2 month2 day2
  in  diffDays date2 date1

type Date = (Integer, Int, Int)
formatDate :: Date -> String
formatDate (year, month, day) =
  show year ++ "-" ++ printf "%02d" month ++ "-" ++ printf "%02d" day

formatNumber :: Double -> String
formatNumber = printf "%10.2f"

formatCurrency :: Double -> String
formatCurrency num = "R$ " ++ formatNumber num

instance Show LoanQueryResult where
  show result =
    let table = show (loanResultTable result)
    in  table
          ++ "\n "
          ++ "\n "
          ++ "\nPrincipal:    "
          ++ formatCurrency (loanResultPrincipal result)
          ++ "\nTax:          "
          ++ formatCurrency (loanResultTax result)
          ++ "\nComms:        "
          ++ formatCurrency (loanResultCommissions result)
          ++ "\nLiquid:       "
          ++ formatCurrency (loanResultLiquidValue result)
          ++ "\nInterest:     "
          ++ formatCurrency (loanResultInterest result)
          ++ "\nTotal PMT:    "
          ++ formatCurrency (loanResultTotal result)
          ++ "\n"

instance Show AmortizationTable where
  show amortizationTable =
    let
      generateRow (AmortizationTableItem date payment interest principal remaining total tax)
        = let paymentStr   = printf "%3d" payment
              dateStr      = formatDate date
              interestStr  = formatCurrency interest
              principalStr = formatCurrency principal
              remainingStr = formatCurrency remaining
              totalStr     = formatCurrency total
              taxStr       = formatCurrency tax
          in  hsep
                2
                left
                [ text paymentStr
                , text dateStr
                , text interestStr
                , text principalStr
                , text remainingStr
                , text totalStr
                , text taxStr
                ]

      -- Generate the rows for the amortization table
      rows   = map generateRow (amTable amortizationTable)

      -- Generate the header row for the amortization table
      header = hsep
        2
        left
        [ text "#  "
        , text "Date      "
        , text "Interest     "
        , text "Principal    "
        , text "Remaining    "
        , text "Payment      "
        , text "Tax          "
        ]

      -- Generate the table
      table = vsep 0 left (header : rows)
    in
      -- Render the table as a string
      render table
