{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}

module LoanCalculator
  ( LoanQuery(..)
  , Frequency(..)
  , Commission(..)
  , CommissionType(..)
  , AmortizationType(..)
  , queryLoan
  , asNetValueLoan
  , LoanQueryResult(..)
  , AmortizationTable(..)
  , AmortizationTableItem(..)
  ) where

{- $>
  let example = asNetValueLoan LoanQuery {
    loanAmount = 31500,
    interestRate = 0.047,
    loanTerms = 6,
    interestRateFrequency = Monthly,
    paymentFrequency = Monthly,
    accrualFrequency = Monthly,
    startDate = Date 2022 02 28,
    gracePeriodInterestTerms = 0,
    gracePeriodPrincipalTerms = 2,
    commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
    amortizationType = SAC,
    paymentDayOfMonth = 20
  }
<$ -}

-- $> putStr (show (queryLoan query))

import           Data.Aeson ( FromJSON, ToJSON )
import           Date (Date(..), formatDate, numberOfDays, Frequency(..), addTerm, fixDate, day)
import           GHC.Generics ( Generic )
import           Text.PrettyPrint.Boxes (hsep, left, render, text, vsep)
import           Text.Printf            (printf)
import           GoalSeek               (goalSeek)

data LoanQuery = LoanQuery
  { loanAmount                :: CurrencyAmount
  , interestRate              :: InterestRate
  , interestRateFrequency          :: Frequency
  , loanTerms                 :: PaymentNo
  , paymentFrequency          :: Frequency
  , accrualFrequency          :: Frequency
  , startDate                 :: Date
  , gracePeriodInterestTerms  :: PaymentNo
  , gracePeriodPrincipalTerms :: PaymentNo
  , commissions               :: [Commission]
  , amortizationType          :: AmortizationType
  , paymentDayOfMonth         :: Int
  }
  deriving (Show, Generic)
instance FromJSON LoanQuery
instance ToJSON LoanQuery

type PaymentNo = Int
type CurrencyAmount = Double
type InterestRate = Double

data AmortizationTableItem = AmortizationTableItem
  { dueDate      :: Date
  , paymentNo :: PaymentNo
  , accruedInterest  :: CurrencyAmount
  , paymentAmortization :: CurrencyAmount
  , remainingPrincipal :: CurrencyAmount
  , dueAmount     :: CurrencyAmount
  , financialTax       :: CurrencyAmount
  }
  deriving (Eq, Show, Read, Generic)
instance FromJSON AmortizationTableItem
instance ToJSON AmortizationTableItem

newtype AmortizationTable = AmortizationTable
  { paymentItems :: [AmortizationTableItem]
  } deriving (Eq, Read, Generic)
instance FromJSON AmortizationTable
instance ToJSON AmortizationTable

data LoanQueryResult = LoanQueryResult
  { resultSchedule       :: AmortizationTable
  , resultNetValue :: CurrencyAmount
  , resultTax         :: CurrencyAmount
  , resultPrincipal   :: CurrencyAmount
  , resultInterest    :: CurrencyAmount
  , resultTotalPayment       :: CurrencyAmount
  , resultCommissions :: CurrencyAmount
  }
  deriving (Eq, Read, Generic)
instance FromJSON LoanQueryResult
instance ToJSON LoanQueryResult

data Commission = Commission
  { commissionType  :: CommissionType
  , commissionValue :: CurrencyAmount
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


-- FV = PV * (1 + CR)
futureValue :: CurrencyAmount -> InterestRate -> PaymentNo -> CurrencyAmount
futureValue pv r n = pv * (1 + compositeRate r n)

-- CR = (1+ r)^n - 1
compositeRate :: InterestRate -> PaymentNo -> InterestRate
compositeRate r n = ((1 + r) ^ n) - 1

-- DR = (1+ r)^ 1 / n - 1
decomposeRate :: InterestRate -> Double -> InterestRate
decomposeRate r n = ( (1 + r) ** (1 / n) ) - 1

-- This function calculates the variable monthly payment for an SAC amortization schedule
-- given the loan amount, interest rate, loan term andd balance
calculatePaymentSac :: CurrencyAmount -> InterestRate -> PaymentNo -> CurrencyAmount -> Double
calculatePaymentSac loanAmount' interestRate' loanTerm balance =
  let amortization = loanAmount' / fromIntegral loanTerm
  in  amortization + interestRate' * balance

-- This function calculates the fixed monthly payment for an PRICE amortization schedule
-- given the loan amount, interest rate, and loan term
calculatePaymentPrice :: CurrencyAmount -> InterestRate -> PaymentNo -> Double
calculatePaymentPrice loanAmount' interestRate' loanTerm =
  let paymentNo' = fromIntegral loanTerm
  in  loanAmount'
        * futureValue interestRate' interestRate' paymentNo'
        / compositeRate interestRate' paymentNo'

-- Sums the financialTax of every AmortizationTableItem in table
sumTax :: AmortizationTable -> Double
sumTax table = foldr (\p acc -> acc + financialTax p) 0 (paymentItems table)

-- Gets the initial remaninig amount and subtract taxes
netValue :: LoanQuery -> AmortizationTable -> Double
netValue loan table =
  remainingPrincipal (head (paymentItems table))
    - sumTax table
    - totalCommissionValue loan

queryLoan :: LoanQuery -> LoanQueryResult
queryLoan loan =
  let accumulate field table' = foldr (\p acc -> acc + field p) 0 (paymentItems table')
      table = generateAmortizationTable loan
  in  LoanQueryResult
        { resultSchedule      = table
        , resultNetValue      = netValue loan table
        , resultTax           = accumulate financialTax table
        , resultPrincipal     = remainingPrincipal (head (paymentItems table))
        , resultInterest      = accumulate accruedInterest table
        , resultTotalPayment  = accumulate dueAmount table
        , resultCommissions   = totalCommissionValue loan
        }

generateAmortizationTable :: LoanQuery -> AmortizationTable
generateAmortizationTable loan = AmortizationTable
  { paymentItems = foldl itemForPaymentNo initialPaymentItems [1 .. loanTerms loan] }
  where
    -- number of payments where amortization > 0
    numberOfPrincipalPayments = loanTerms loan - gracePeriodPrincipalTerms loan

    -- remainingPrincipal at the fixed start date
    initialRemainingPrincipal = remainingPrincipal $ last initialPaymentItems

    -- the rate adjusted to the accrual frequency
    accrualRate = composeRate (interestRate loan) (interestRateFrequency loan) (accrualFrequency loan)

    -- the interest for a given balance
    currentInterest :: CurrencyAmount -> Double
    currentInterest balance = balance * accrualRate

    -- Compute the tax for the given payment number in the loan schedule
    tax :: PaymentNo -> CurrencyAmount -> Double
    tax = brazilianIofTax 0.0038 0.000082

    -- BR IOF calculation dynamics for a given payment
    brazilianIofTax baseTax dailyTax paymentNo' amortization' =
      let startDate'  = startDate loan
          dueDate'    = addTerm startDate' (paymentFrequency loan) paymentNo'
          days        = fromIntegral (numberOfDays startDate' dueDate')
          cappedDays  = min 365 days
      in  (baseTax * amortization')
            + (dailyTax * cappedDays * amortization')

    -- Create the initial amortization table item
    initialPaymentItem :: AmortizationTableItem
    initialPaymentItem = AmortizationTableItem { 
      dueDate      = startDate loan
      , paymentNo = 0
      , accruedInterest  = 0
      , paymentAmortization = 0
      , remainingPrincipal = loanAmount loan
      , dueAmount     = 0
      , financialTax       = 0
      }

    -- Create the initial payment items based on paymentDayOfMonth
    initialPaymentItems = if paymentDayOfMonth loan == day (startDate loan)
        then [initialPaymentItem] else
          initials where
            fixedDate = fixDate (startDate loan) (paymentDayOfMonth loan)
            diffInDays = numberOfDays (startDate loan) fixedDate
            dailyRate = composeRate (interestRate loan) (interestRateFrequency loan) Daily
            rateForPeriod = compositeRate dailyRate (fromIntegral diffInDays)
            interest = rateForPeriod * loanAmount loan
            initials = [initialPaymentItem, initialPaymentItem {
              paymentNo = 1
              , dueDate = fixedDate
              , accruedInterest = interest
              , remainingPrincipal = loanAmount loan + interest
            }]

    itemForPaymentNo prevItems paymentNo' =
      let
        prevItem = last prevItems
        currentPaymentNo = paymentNo prevItem + 1
        balance = remainingPrincipal prevItem
        interest = currentInterest balance
        payment = pmt paymentNo' balance
        frequency = paymentFrequency loan
        previousDate = dueDate prevItem
        dueDate' = addTerm previousDate frequency 1
        amort = max 0 (payment - interest)
      in
      prevItems ++ [AmortizationTableItem
      { dueDate              = dueDate'
      , dueAmount            = payment
      , paymentNo            = currentPaymentNo
      , accruedInterest      = interest
      , paymentAmortization  = max amort 0
      , remainingPrincipal   = balance + interest - payment
      , financialTax         = tax paymentNo' amort
      }]

    -- Compute the payment for a given payment number in the schedule
    pmt :: PaymentNo -> CurrencyAmount -> Double
    pmt paymentNo' balance
      | paymentNo' <= gracePeriodInterestTerms loan = 0
      | paymentNo' <=  gracePeriodPrincipalTerms loan = currentInterest balance
      | otherwise = payment
      where
        startValue = futureValue initialRemainingPrincipal accrualRate (gracePeriodInterestTerms loan)
        payment = case amortizationType loan of
          SAC -> calculatePaymentSac startValue accrualRate numberOfPrincipalPayments balance
          PRICE -> calculatePaymentPrice startValue accrualRate numberOfPrincipalPayments
                                    
-- Calculate the total value of the given list of commissions
totalCommissionValue :: LoanQuery -> Double
totalCommissionValue loanQuery =
  let absoluteValue (Commission Absolute value)     = value
      absoluteValue _                               = 0.0
      percentageValue (Commission Percentage value) = value
      percentageValue _                             = 0.0
      commissionValues = map (\c -> absoluteValue c + (percentageValue c / 100) * loanAmount loanQuery) $ commissions loanQuery
  in sum commissionValues

-- Convert a LoanQuery into another where the netValue of the later is equal to the amount of the first
asNetValueLoan :: LoanQuery -> Maybe LoanQuery
asNetValueLoan loan =
  let target = loanAmount loan
      netValueQuery x = target - netValue loan {loanAmount = x } (resultSchedule $ queryLoan loan {loanAmount = x })
      tolerance = 0.49
      maxIterations = 300
      res   = goalSeek netValueQuery tolerance target maxIterations
  in  case res of
    Nothing -> Nothing
    Just res' -> Just $ loan { loanAmount = res' }

-- compose a rate from given frequency to frequency
composeRate :: InterestRate -> Frequency -> Frequency -> InterestRate
composeRate rate Monthly Monthly = rate
composeRate rate Biweekly Biweekly = rate
composeRate rate Weekly Weekly = rate
composeRate rate Daily Daily = rate
composeRate rate Yearly Yearly = rate

composeRate rate Daily Weekly   = compositeRate rate 7
composeRate rate Daily Biweekly   = compositeRate rate 14
composeRate rate Daily Monthly  = compositeRate rate 30
composeRate rate Daily Yearly   = compositeRate rate 360

composeRate rate Weekly Daily = decomposeRate rate 7
composeRate rate Weekly Biweekly  = compositeRate rate 2
composeRate rate Weekly Monthly = 1 + rate ** 4.2
composeRate rate Weekly Yearly = 1 + rate ** 51.42

composeRate rate Biweekly Daily = decomposeRate rate 14
composeRate rate Biweekly Weekly = compositeRate rate 2
composeRate rate Biweekly Monthly = 1 + rate ** 2.1
composeRate rate Biweekly Yearly = 1 + rate ** 25.71

composeRate rate Monthly Daily = decomposeRate rate 30
composeRate rate Monthly Weekly  = decomposeRate rate 4.2
composeRate rate Monthly Biweekly   = decomposeRate rate 2
composeRate rate Monthly Yearly    = compositeRate rate 12

composeRate rate Yearly Monthly = decomposeRate rate 12
composeRate rate Yearly Daily = decomposeRate rate 360
composeRate rate Yearly Weekly = decomposeRate rate 51.42
composeRate rate Yearly Biweekly  = decomposeRate rate 25.71



formatNumber :: Double -> String
formatNumber = printf "%10.2f"

formatCurrency :: Double -> String
formatCurrency num = "R$ " ++ formatNumber num

instance Show LoanQueryResult where
  show result =
    let table = show (resultSchedule result)
    in  table
          ++ "\n "
          ++ "\n "
          ++ "\nPrincipal:    "
          ++ formatCurrency (resultPrincipal result)
          ++ "\nTax:          "
          ++ formatCurrency (resultTax result)
          ++ "\nComms:        "
          ++ formatCurrency (resultCommissions result)
          ++ "\nNet:       "
          ++ formatCurrency (resultNetValue result)
          ++ "\nInterest:     "
          ++ formatCurrency (resultInterest result)
          ++ "\nTotal PMT:    "
          ++ formatCurrency (resultTotalPayment result)
          ++ "\n"

instance Show AmortizationTable where
  show amortizationTable' =
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
      rows   = map generateRow (paymentItems amortizationTable')

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
