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
  , asLiquidValueLoan
  , LoanQueryResult(..)  
  , AmortizationTable(..)
  , AmortizationTableItem(..)
  ) where

import           Data.Aeson ( FromJSON, ToJSON )
import           Date (Date(..), formatDate, numberOfDays, Frequency(..), addTerm)
import           GHC.Generics ( Generic )
import           Text.PrettyPrint.Boxes (hsep, left, render, text, vsep)
import           Text.Printf            (printf)
import           GoalSeek               (goalSeek)     
      
data LoanQuery = LoanQuery
  { loanAmount       :: Double
  , interestRate         :: Double
  , loanTerms        :: Int
  , paymentFrequency    :: Frequency
  , startDate    :: Date
  , gracePeriodInterestTerms  :: Int
  , gracePeriodPrincipalTerms :: Int
  , commissions  :: [Commission]
  , amortizationType  :: AmortizationType
  }
  deriving (Show, Generic)
instance FromJSON LoanQuery
instance ToJSON LoanQuery

data AmortizationTableItem = AmortizationTableItem
  { dueDate      :: Date
  , paymentNo :: Int
  , accruedInterest  :: Double
  , paymentAmortization :: Double
  , remainingPrincipal :: Double
  , dueAmount     :: Double
  , financialTax       :: Double
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
  , resultLiquidValue :: Double
  , resultTax         :: Double
  , resultPrincipal   :: Double
  , resultInterest    :: Double
  , resultTotalPayment       :: Double
  , resultCommissions :: Double
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

type PaymentNo = Int
type CurrencyAmount = Double
type InterestRate = Double

-- FV = PV * CR
futureValue :: CurrencyAmount -> InterestRate -> PaymentNo -> CurrencyAmount
futureValue pv r n = pv * compositeRate r n

-- CR = (1+ r)^n
compositeRate :: InterestRate -> PaymentNo -> InterestRate
compositeRate r n = (1 + r) ^ n

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
        / (compositeRate interestRate' paymentNo' - 1)

-- Sums the financialTax of every AmortizationTableItem in table
sumTax :: AmortizationTable -> Double
sumTax table = foldr (\p acc -> acc + financialTax p) 0 (paymentItems table)

-- Gets the initial remaninig amount and subtract taxes
liquidValue :: LoanQuery -> AmortizationTable -> Double
liquidValue loan table =
  remainingPrincipal (head (paymentItems table))
    - sumTax table
    - totalCommissionValue loan

queryLoan :: LoanQuery -> LoanQueryResult
queryLoan loan =
  let accumulate field table' = foldr (\p acc -> acc + field p) 0 (paymentItems table')      
      table = generateAmortizationTable loan
  in  LoanQueryResult
        { resultSchedule   = table
        , resultLiquidValue   = liquidValue loan table
        , resultTax           = accumulate financialTax table
        , resultPrincipal     = remainingPrincipal (head (paymentItems table))
        , resultInterest      = accumulate accruedInterest table
        , resultTotalPayment  = accumulate dueAmount table
        , resultCommissions   = totalCommissionValue loan
        }

{- $>
  let query = asLiquidValueLoan LoanQuery {
    loanAmount = 31500,
    interestRate = 0.047,
    loanTerms = 6,
    paymentFrequency = Monthly,
    startDate = (2022, 02, 28),
    gracePeriodInterestTerms = 0,
    gracePeriodPrincipalTerms = 2,
    commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
    amortizationType = SAC
  }
<$ -}

-- $> putStr (show (queryLoan query))


generateAmortizationTable :: LoanQuery -> AmortizationTable
generateAmortizationTable loan = AmortizationTable
  { paymentItems = scanl calculatePaymentItem initial [1 .. loanTerms loan] }
  where
    -- Compute the payment for a given payment number in the schedule
    pmt :: PaymentNo -> CurrencyAmount -> Double
    pmt paymentNo' balance      
      | paymentNo' <= gracePeriodPrincipalTerms loan = currentInterest balance
      | paymentNo' <= gracePeriodInterestTerms loan = 0
      | otherwise = payment
      where
        payment = case amortizationType loan of
          SAC -> calculatePaymentSac startValue ir npp balance
          PRICE -> calculatePaymentPrice startValue ir npp
        ir = interestRate loan
        npp = numberOfPrincipalPayments
        startValue = futureValue (loanAmount loan)
                          (interestRate loan)
                          (gracePeriodInterestTerms loan)
          
    numberOfPrincipalPayments = loanTerms loan - gracePeriodPrincipalTerms loan

    -- Compute the interest for a given balance
    currentInterest :: CurrencyAmount -> Double
    currentInterest balance = balance * interestRate loan

    -- Compute the amount that was subtracted from the principal after this payment
    amortization :: PaymentNo -> CurrencyAmount -> Double
    amortization paymentNo' balance = pmt paymentNo' balance - currentInterest balance

    -- Compute the tax for the given payment number in the loan schedule
    tax :: PaymentNo -> CurrencyAmount -> Double
    tax = brazilianIofTax 0.0038 0.000082 

    -- BR IOF calculation dynamics for a given payment
    brazilianIofTax baseTax dailyTax paymentNo' balance =
      let startDate'  = startDate loan
          dueDate'    = addTerm startDate' (paymentFrequency loan) paymentNo'
          am         = amortization paymentNo' balance
          days       = fromIntegral (numberOfDays startDate' dueDate')
          cappedDays = min 365 days
          noPayments = fromIntegral numberOfPrincipalPayments
      in  ((baseTax * loanAmount loan) / noPayments)
            + (dailyTax * cappedDays * am)

    -- Create the initial amortization table item
    initial :: AmortizationTableItem
    initial = AmortizationTableItem { dueDate      = startDate loan
                                    , paymentNo = 0
                                    , accruedInterest  = 0
                                    , paymentAmortization = 0
                                    , remainingPrincipal = loanAmount loan
                                    , dueAmount     = 0
                                    , financialTax       = 0
                                    }

    itemForPaymentNo paymentNo' prevItem =
      let
        balance = remainingPrincipal prevItem
        interest = currentInterest balance
        payment = pmt paymentNo' balance
        amort = amortization paymentNo' balance
        initialDate = startDate loan
        frequency = paymentFrequency loan
      in
      AmortizationTableItem
      { dueDate              = addTerm initialDate frequency paymentNo'
      , paymentNo            = paymentNo'
      , accruedInterest      = interest
      , paymentAmortization  = max amort 0
      , remainingPrincipal   = balance + interest - payment
      , dueAmount         = payment
      , financialTax         = tax paymentNo' balance
      }

    calculatePaymentItem prevItem paymentNo' = itemForPaymentNo paymentNo' prevItem

-- Calculate the total value of the given list of commissions
totalCommissionValue :: LoanQuery -> Double
totalCommissionValue loan =
  let commissions'                                  = commissions loan
      absoluteValue (Commission Absolute   value)   = value
      absoluteValue (Commission Percentage _    )   = 0.0
      percentageValue (Commission Absolute   _    ) = 0.0
      percentageValue (Commission Percentage value) = value
      absTotal                                      = sum $ map absoluteValue commissions'
      percTotal                                     = sum $ map percentageValue commissions'
  in  absTotal + ((percTotal / 100) * loanAmount loan)

-- Convert a LoanQuery into another where the liquidValue of the later is equal to the amount of the first
asLiquidValueLoan :: LoanQuery -> LoanQuery
asLiquidValueLoan loan =
  let query = testAmountForLiquidAmount loan
      res   = goalSeek query 0.001 (loanAmount loan) 250
  in  loan { loanAmount       = res }

-- | Returns the liquid value difference between the original query and the test query
testAmountForLiquidAmount :: LoanQuery -> Double -> Double
testAmountForLiquidAmount loan n =
  let testLoan = loan {loanAmount = n }
      testLiquidValue = liquidValue testLoan (resultSchedule $ queryLoan testLoan)
  in loanAmount loan - testLiquidValue

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
          ++ "\nLiquid:       "
          ++ formatCurrency (resultLiquidValue result)
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
