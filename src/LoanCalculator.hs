{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use if" #-}

module LoanCalculator
  ( LoanQuery(..)
  , Frequency(..)
  , Commission
  , CommissionType
  , queryLoan
  , asLiquidValueLoan
  , LoanQueryResult
  ) where

import           Data.Aeson
import           Data.Time.Calendar             ( addDays
                                                , addGregorianMonthsClip
                                                , addGregorianYearsClip
                                                , diffDays
                                                , fromGregorian
                                                , toGregorian
                                                )
import           GHC.Generics
import           Text.PrettyPrint.Boxes         ( hsep
                                                , left
                                                , render
                                                , text
                                                , vsep
                                                )
import           Text.Printf                    ( printf )

data LoanQuery = LoanQuery
  { amQueryAmount       :: Double
  , amQueryRate         :: Double
  , amQueryTerms        :: Int
  , amQueryFrequency    :: Frequency
  , amQueryStartDate    :: Date
  , amQueryAbsInterest  :: Int
  , amQueryAbsPrincipal :: Int
  , amQueryCommissions  :: [Commission]
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

data Frequency =
  Monthly |
  Biweekly |
  Weekly |
  Daily |
  Yearly
  deriving (Enum, Show, Generic)
instance FromJSON Frequency
instance ToJSON Frequency

-- FV = PV(1+ r)^n
futureValue :: Double -> Double -> Int -> Double
futureValue pv r n = pv * compositeRate r n

compositeRate :: Double -> Int -> Double
compositeRate r n = (1 + r) ^ n

-- This function calculates the monthly payment for an amortization schedule
-- given the loan amount, interest rate, and loan term
calculatePayment :: Double -> Double -> Int -> Double
calculatePayment loanAmount interestRate loanTerm =
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
    -- It allows three states for amortization: 
    --  absolving principal with and interest table: amQueryAbsInterest, amQueryAbsPrincipal
    --  absolving principal table: amQueryAbsPrincipal
    --  PRICE
    -- amQueryAbsPrincipal must be > than amQueryAbsInterest
    pmt paymentNo = case paymentNo > amQueryAbsPrincipal loan of
      -- There is a payment of principal and interest
      True ->
        calculatePayment startValue (amQueryRate loan) numberOfPrincipalPayments
      False -> case paymentNo > amQueryAbsInterest loan of
        -- There is a payment of interest
        True  -> currentInterest paymentNo
        -- There is no payment
        False -> 0

    numberOfPrincipalPayments = amQueryTerms loan - amQueryAbsPrincipal loan

    -- The interest for given paymentNo
    currentInterest paymentNo = currentBalance paymentNo * amQueryRate loan

    -- The amount left to pay in the start of a given period of time
    currentBalance paymentNo = remainingBalance (paymentNo - 1)

    -- The amount left to debitor pay after the payment is performed
    -- if no payment, the remainingBalane should be > currentBalance
    remainingBalance paymentNo = case paymentNo of
      -- The debit starting point is the amount borrowed
      0 -> amQueryAmount loan
      _ -> case paymentNo < amQueryAbsInterest loan of
        -- No payment was performed yet, calculates the future value of the debit to the given paymentNo
        True -> futureValue (amQueryAmount loan) (amQueryRate loan) paymentNo
        -- Calculate the balance considering previous payments and current payment
        -- Get last balance, add the interest, subtract the payment
        False ->
          currentBalance paymentNo + currentInterest paymentNo - pmt paymentNo

    -- The value when is due the first payment of principal
    startValue = futureValue (amQueryAmount loan)
                             (amQueryRate loan)
                             (amQueryAbsInterest loan)

    -- The amount that was subtracted from the principal after this paymentNo (can be negative)
    amortization paymentNo = pmt paymentNo - currentInterest paymentNo

    -- Taxes for a given payment
    tax paymentNo = case amortization paymentNo > 0 of
      True  -> brazilianIofTax 0.0038 0.000082 paymentNo
      False -> 0

    -- BR IOF calculation dynamics for a given payment (@ToDo: parametrize over the constants)
    brazilianIofTax baseTax dailyTax paymentNo =
      let startDate  = amQueryStartDate loan
          dueDate    = addTerm startDate (amQueryFrequency loan) paymentNo
          am         = amortization paymentNo
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

    itemForPaymentNo paymentNo = AmortizationTableItem
      { amTableDate      = addTerm (amQueryStartDate loan)
                                   (amQueryFrequency loan)
                                   paymentNo
      , amTablePaymentNo = paymentNo
      , amTableInterest  = currentInterest paymentNo
      , amTablePrincipal = max (amortization paymentNo) 0
      , amTableRemaining = remainingBalance paymentNo
      , amTableTotal     = pmt paymentNo
      , amTableTax       = tax paymentNo
      }
  in
    AmortizationTable
      { amTable = initial
                    : [ itemForPaymentNo paymentNo
                      | paymentNo <- [1 .. amQueryTerms loan]
                      ]
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
                           }
      val = liquidValue testLoan (loanResultTable $ queryLoan testLoan)
  in  amQueryAmount loan - val

-- This function adds the given number of periods of certain frequency to the given date and returns the resulting date
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
