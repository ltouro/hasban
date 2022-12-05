{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module AmortizationTable
  ( AmortizationOption(..)
  , QueryLoan(..)
  , queryLoan
  , LoanQueryResult
  ) where

import           Data.Time.Calendar             ( addDays
                                                , addGregorianMonthsClip
                                                , addGregorianYearsClip
                                                , diffDays
                                                , fromGregorian
                                                , toGregorian
                                                )
import           Text.PrettyPrint.Boxes
import           Text.Printf                    ( printf )

data QueryLoan = QueryLoan
  { amQueryAmount       :: Double
  , amQueryRate         :: Double
  , amQueryTerms        :: Int
  , amQueryFrequency    :: AmortizationOption
  , amQueryStartDate    :: Date
  , amQueryAbsInterest  :: Int
  , amQueryAbsPrincipal :: Int
  }
  deriving Show

data AmortizationTableItem = AmortizationTableItem
  { amTableDate      :: Date
  , amTablePaymentNo :: Int
  , amTableInterest  :: Double
  , amTablePrincipal :: Double
  , amTableRemaining :: Double
  , amTableTotal     :: Double
  , amTableTax       :: Double
  }
  deriving Show

type AmortizationTable = [AmortizationTableItem]

data LoanQueryResult = LoanQueryResult
  { loanResultTable       :: AmortizationTable
  , loanResultLiquidValue :: Double
  , loanResultTax         :: Double
  , loanResultPrincipal   :: Double
  , loanResultInterest    :: Double
  , loanResultTotal       :: Double
  }

data AmortizationOption =
  Monthly |
  Biweekly |
  Weekly |
  Daily |
  Yearly
  deriving (Enum, Show)


type Date = (Integer, Int, Int)
format :: Date -> String
format (year, month, day) =
  show year ++ "-" ++ printf "%02d" month ++ "-" ++ printf "%02d" day


formatNumber :: Double -> String
formatNumber = printf "%10.2f"

formatCurrency :: Double -> String
formatCurrency num = "R$ " ++ formatNumber num

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

queryLoan :: QueryLoan -> LoanQueryResult
queryLoan loan =
  let accum field = foldr (\p acc -> acc + field p) 0
      table = generateAmortizationTable loan
  in  LoanQueryResult { loanResultTable       = table
                      , loanResultLiquidValue = liquidValue table
                      , loanResultTax         = accum amTableTax table
                      , loanResultPrincipal   = accum amTablePrincipal table
                      , loanResultInterest    = accum amTableInterest table
                      , loanResultTotal       = accum amTableTotal table
                      }

-- This function generates an amortization table, given the loan amount,
-- interest rate, paymentFrequency, and loan term (in AmortizationOption)
generateAmortizationTable :: QueryLoan -> AmortizationTable
generateAmortizationTable loan = -- generateAmortizationTable (amQueryAmount loan) (amQueryRate loan) (amQueryTerms loan) (amQueryFrequency loan) (amQueryStartDate loan)
  let
    periodicPaymentWithAbs paymentNo =
      case paymentNo > amQueryAbsPrincipal loan of
        True  -> periodicPayment
        False -> case paymentNo > amQueryAbsInterest loan of
          True  -> currentBalance paymentNo * amQueryRate loan
          False -> 0

    periodicPayment = calculatePayment
      (startValue loan)
      (amQueryRate loan)
      (amQueryTerms loan - amQueryAbsPrincipal loan)

    -- This function calculates the remaining balance on a loan, given
    -- the loan amount, interest rate, and number of payments made
    remainingBalance paymentNo =
      startValue loan
        * (1 + amQueryRate loan)
        ^ paymentNo
        - periodicPaymentWithAbs paymentNo
        * ((1 + amQueryRate loan) ^ paymentNo - 1)
        / amQueryRate loan

    currentBalance paymentNo = remainingBalance (paymentNo - 1)

    startValue loan = futureValue (amQueryAmount loan)
                                  (amQueryRate loan)
                                  (amQueryAbsInterest loan)

    amortization paymentNo =
      periodicPaymentWithAbs paymentNo
        - (currentBalance paymentNo * amQueryRate loan)

    taxOnAmortization startDate paymentFrequency paymentNo =
      case amortization paymentNo > 0 of
        True  -> tax startDate paymentFrequency paymentNo
        False -> 0

    tax startDate paymentFrequency paymentNo =
      let days = fromIntegral
            ( numberOfDays startDate
            $ addTerm startDate paymentFrequency paymentNo
            )
          cappedDays = min 365 days
          baseTax    = 0.0038
          dailyTax   = 0.000082
      in  (baseTax * amortization paymentNo)
            + (dailyTax * cappedDays * amortization paymentNo)

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
      , amTableInterest  = currentBalance paymentNo * amQueryRate loan
      , amTablePrincipal = periodicPaymentWithAbs paymentNo
                             - (currentBalance paymentNo * amQueryRate loan)
      , amTableRemaining = remainingBalance paymentNo
      , amTableTotal     = periodicPaymentWithAbs paymentNo
      , amTableTax       = taxOnAmortization (amQueryStartDate loan)
                                             (amQueryFrequency loan)
                                             paymentNo
      }
  in
    -- Generate a AmortizationTableItem for each payment
    ( initial
    : [ itemForPaymentNo paymentNo | paymentNo <- [1 .. amQueryTerms loan] ]
    )

instance Show LoanQueryResult where
  show result =
    let table = prettyPrintAmortizationTable (loanResultTable result)
    in  table
          ++ "\n "
          ++ "\nTotais"
          ++ "\n "
          ++ "\nLiquid:       "
          ++ formatCurrency (loanResultLiquidValue result)
          ++ "\nTax:          "
          ++ formatCurrency (loanResultTax result)
          ++ "\nPrincipal:    "
          ++ formatCurrency (loanResultPrincipal result)
          ++ "\nInterest:     "
          ++ formatCurrency (loanResultInterest result)
          ++ "\nTotal PMT:    "
          ++ formatCurrency (loanResultTotal result)
          ++ "\n"

-- This function takes a AmortizationTable and formats it in a more readable way
prettyPrintAmortizationTable :: AmortizationTable -> String
prettyPrintAmortizationTable amortizationTable =
  let --      

      -- This function generates a string for a single row in the amortization table
      -- given the values for that row
    generateRow (AmortizationTableItem date payment interest principal remaining total tax)
      = let paymentStr   = printf "%3d" payment
            dateStr      = format date
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
    rows   = map generateRow amortizationTable

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

-- This function adds the given number of periods of certain frequency to the given date and returns the resulting date
addTerm :: Date -> AmortizationOption -> Int -> Date
addTerm (year, month, day) paymentFrequency numPeriods =
  let gregorianDate = fromGregorian year month day
      numDays       = fromIntegral $ case paymentFrequency of
        Monthly  -> numPeriods
        Biweekly -> numPeriods * 14
        Weekly   -> numPeriods * 7
        Daily    -> numPeriods
        Yearly   -> numPeriods
      newDate = case paymentFrequency of
        Monthly -> addGregorianMonthsClip numDays gregorianDate
        Yearly  -> addGregorianYearsClip numDays gregorianDate
        _       -> addDays numDays gregorianDate
      (newYear, newMonth, newDay) = toGregorian newDate
  in  (fromIntegral newYear, newMonth, newDay)

numberOfDays :: Date -> Date -> Integer
numberOfDays (year1, month1, day1) (year2, month2, day2) =
  let date1 = fromGregorian year1 month1 day1
      date2 = fromGregorian year2 month2 day2
  in  diffDays date2 date1

-- Sums the amTableTax of every AmortizationTableItem in table
sumTax :: AmortizationTable -> Double
sumTax = foldr (\p acc -> acc + amTableTax p) 0

liquidValue :: AmortizationTable -> Double
liquidValue table = amTableRemaining (head table) - sumTax table

{- $>
  let query = QueryLoan { 
    amQueryAmount = 30000,  
    amQueryRate = 0.05,  
    amQueryTerms = 34, 
    amQueryFrequency = Monthly, 
    amQueryStartDate = (2022, 12, 30),
    amQueryAbsInterest = 1,
    amQueryAbsPrincipal = 2 }
<$ -}

-- $> let table = generateAmortizationTable query

-- $> putStr (show (queryLoan query))
