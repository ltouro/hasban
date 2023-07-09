{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use isJust" #-}
{-# LANGUAGE GADTs             #-}

module LoanCalculator
  ( LoanQuery(..)
  , LoanQueryValidationError
  , Frequency(..)
  , Commission(..)
  , CommissionType(..)
  , AmortizationType(..)
  , calculate
  , validateAndCalculate
  , asNetValueLoan
  , LoanQueryResult(..)
  , AmortizationTable(..)
  , AmortizationTableItem(..)
  , InterestMode(..)
  , CurrencyAmount
  , PaymentNo
  , PersonType(..)
  ) where


import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Maybe             (catMaybes, fromMaybe, isJust)
import           Date                   (Date (..), Frequency (..), addTerm,
                                         formatDate, numberOfDays,
                                         numberOfMonths)

import           Control.Monad.State
import           GHC.Generics           (Generic)
import           GoalSeek               (goalSeek)
import           InterestFactor         (InterestMode (..),
                                         InterestPeriod (Day, Month),
                                         InterestRate,
                                         interestFactorForPaymentNo,
                                         interestUnitValue, yearlyRate)
import           IRR                    (calculateIrr)
import           Text.PrettyPrint.Boxes (hsep, left, render, text, vsep)
import           Text.Printf            (printf)

data LoanQuery = LoanQuery
  { loanAmount                :: CurrencyAmount
  , interestRate              :: InterestRate
  , interestMode              :: InterestMode
  , loanTerms                 :: PaymentNo
  , paymentFrequency          :: Frequency
  , paymentEveryX             :: Maybe PaymentNo
  , startDate                 :: Date
  , firstIncorporationEvent   :: Maybe Date
  , principalGracePeriodTerms :: PaymentNo
  , commissions               :: [Commission]
  , amortizationType          :: AmortizationType
  , personType                :: PersonType
  }
  deriving (Show, Generic)
instance FromJSON LoanQuery
instance ToJSON LoanQuery

interestRatePct :: LoanQuery -> InterestRate
interestRatePct l = interestRate l / 100.00

isRegular :: LoanQuery -> Bool
isRegular loan = case (startDate loan, firstIncorporationEvent loan) of
  (Date d _ _, Just (Date d' _ _)) -> d == d'
  _                                -> True

hasIncorporationEvent :: LoanQuery -> Bool
hasIncorporationEvent loan = isJust (firstIncorporationEvent loan)

baseDate :: LoanQuery -> Date
baseDate loan = fromMaybe (startDate loan) (firstIncorporationEvent loan)

currentDate :: LoanQuery -> PaymentNo -> Date
currentDate loan paymentNo'
  | paymentNo' == 0                               = startDate loan
  | paymentNo' == 1 && hasIncorporationEvent loan = baseDate loan
  | otherwise                                     = addTerm (currentDate loan $ paymentNo' - 1)
                                                        (paymentFrequency loan) $ termsPerPayment loan


numberOfPayments :: LoanQuery -> PaymentNo
numberOfPayments loan
  | firstIncorporationEvent loan /= Nothing = loanTerms loan + 1
  | otherwise = loanTerms loan

termsPerPayment :: LoanQuery -> PaymentNo
termsPerPayment loan = fromMaybe 1 $ paymentEveryX loan

type PaymentNo = Int
type CurrencyAmount = Double

data PersonType = Legal | Natural
  deriving (Eq, Show, Read, Generic)
instance FromJSON PersonType
instance ToJSON PersonType

data AmortizationTableItem = AmortizationTableItem
  { dueDate             :: Date
  , paymentNo           :: PaymentNo
  , accruedInterest     :: CurrencyAmount
  , paymentAmortization :: CurrencyAmount
  , remainingPrincipal  :: CurrencyAmount
  , dueAmount           :: CurrencyAmount
  , financialTax        :: CurrencyAmount
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
  { resultSchedule               :: AmortizationTable
  , resultNetValue               :: CurrencyAmount
  , resultTax                    :: CurrencyAmount
  , resultStartPrincipal         :: CurrencyAmount
  , resultFinalPrincipal         :: CurrencyAmount
  , resultInterest               :: CurrencyAmount
  , resultTotalPayment           :: CurrencyAmount
  , resultCommissions            :: CurrencyAmount
  , resultYearlyEffectiveRate    :: InterestRate
  , resultMonthlyEffectiveRate   :: InterestRate
  , resultMonthlyNominalRate     :: InterestRate
  , resultYearlyNominalRate      :: InterestRate
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

data CommissionType = Absolute | Percentage
  deriving (Show, Generic)

instance FromJSON CommissionType
instance ToJSON CommissionType

data AmortizationType =
    PRICE |
    SAC
  deriving (Enum, Show, Generic)
instance FromJSON AmortizationType
instance ToJSON AmortizationType

data LoanQueryValidationError =
  InvalidStartDate
  | InvalidGracePeriod
  | InvalidInterestRate
  | InvalidPaymentNo
  | InvalidCommission
  | InvalidAmount
  | InvalidFrequency
  | InvalidPaymentEveryX
  deriving (Show, Generic)
instance FromJSON LoanQueryValidationError
instance ToJSON LoanQueryValidationError

-- FV = PV * (1 + CR)
futureValue ∷ CurrencyAmount → InterestRate → PaymentNo → CurrencyAmount
futureValue pv r n = pv * (1 + compositeRate r n)

-- CR = (1+ r)^n - 1
compositeRate ∷ InterestRate → PaymentNo → InterestRate
compositeRate r n = ((1 + r) ^ n) - 1

-- This function calculates the variable monthly payment for an SAC amortization schedule
-- given the loan amount, interest rate, loan term andd balance
calculatePaymentSac ∷ CurrencyAmount → PaymentNo → Double
calculatePaymentSac loanAmount' loanTerm = loanAmount' / fromIntegral loanTerm

-- This function calculates the fixed monthly payment for an PRICE amortization schedule
-- given the loan amount, interest rate, and loan term
calculatePaymentPrice ∷ CurrencyAmount → InterestRate → PaymentNo → Double
calculatePaymentPrice loanAmount' r loanTerm =
  let
    paymentNo' = fromIntegral loanTerm
    res = loanAmount'
        * futureValue r r paymentNo'
        / compositeRate r paymentNo'
  in
    res

-- Sums the financialTax of every AmortizationTableItem in table
sumTax ∷ AmortizationTable → Double
sumTax table = foldr (\p acc -> acc + financialTax p) 0 (paymentItems table)

interestPeriod :: LoanQuery -> PaymentNo -> InterestPeriod
interestPeriod loan paymentNo'
  | paymentNo' == 1 && hasIncorporationEvent loan && not (isRegular loan) = Day
  | otherwise = case interestMode loan of
    ThreeSixtyFiveMonth -> Month
    ThreeSixtyMonth     -> Month
    TwoFiveTwoMonth     -> Month
    _                   -> Day

-- Gets the initial remaninig amount and subtract taxes
netValue ∷ LoanQuery → AmortizationTable → Double
netValue loan table =
  remainingPrincipal (head (paymentItems table))
    - sumTax table
    - totalCommissionValue loan

initialRemainingPrincipal :: LoanQuery -> CurrencyAmount
initialRemainingPrincipal loan
      | firstIncorporationEvent loan /= Nothing = loanAmount loan + interestForPaymentNo loan 1 (loanAmount loan)
      | otherwise = loanAmount loan

numberOfTerms :: LoanQuery -> PaymentNo -> Integer
numberOfTerms loan paymentNo' =  let
    previousDate = currentDate loan (paymentNo' - 1)
    currentDate' = currentDate loan paymentNo'
    terms = case interestPeriod loan paymentNo' of
      Day   -> numberOfDays previousDate currentDate'
      Month -> min 1 (numberOfMonths previousDate currentDate')
    in terms

interestForPaymentNo :: LoanQuery -> PaymentNo -> CurrencyAmount -> CurrencyAmount
interestForPaymentNo loan paymentNo' remainingPrincipal' =
  let
    mode = interestMode loan
    terms = numberOfTerms loan paymentNo'
    interestFactor = interestFactorForPaymentNo mode (interestRatePct loan) terms
    interest = interestUnitValue remainingPrincipal' interestFactor
  in
    interest

isGracePeriod :: LoanQuery -> PaymentNo -> Bool
isGracePeriod loan paymentNo' = paymentNo' <= (principalGracePeriodTerms loan + initialPaymentCount loan - 1)

-- Compute the payment for a given payment number in the schedule
pmt ∷ LoanQuery -> PaymentNo → CurrencyAmount → Double
pmt loan paymentNo' balance
  | isGracePeriod loan paymentNo' = interestForPaymentNo loan paymentNo' balance
  | otherwise = payment
  where
    interest = interestForPaymentNo loan paymentNo' balance
    payment =  case amortizationType loan of
      SAC   -> fixedPayment loan + interest
      PRICE -> fixedPayment loan

initialPaymentCount :: Num p => LoanQuery -> p
initialPaymentCount loan = if hasIncorporationEvent loan then 2 else 1

numberOfPrincipalPayments :: LoanQuery -> PaymentNo
numberOfPrincipalPayments loan = loanTerms loan - principalGracePeriodTerms loan

-- Compute the tax for the given payment number in the loan schedule
tax ∷ LoanQuery -> PaymentNo → CurrencyAmount → Double
tax loan = let
  baseTax = 0.0038
  dailyTax = case personType loan of
    Natural -> 0.000082
    Legal   -> 0.000041
  in brazilianIofTax loan baseTax dailyTax

 -- BR IOF calculation dynamics for a given payment
brazilianIofTax :: (Ord a, Num a) => LoanQuery -> a -> a -> PaymentNo -> a -> a
brazilianIofTax loan baseTax dailyTax paymentNo' amortization' =
  let startDate'  = startDate loan
      dueDate'    = addTerm startDate' (paymentFrequency loan) paymentNo'
      days        = fromIntegral (numberOfDays startDate' dueDate')
      cappedDays  = min 365 days
  in  (baseTax * amortization')
        + (dailyTax * cappedDays * amortization')

-- The payment necessary to pay the loan given a amortizationType
fixedPayment :: LoanQuery -> Double
fixedPayment loan = case amortizationType loan of
      SAC -> calculatePaymentSac (initialRemainingPrincipal loan) (numberOfPrincipalPayments loan)
      PRICE -> let
        terms = numberOfTerms loan (initialPaymentCount loan)
        mode = interestMode loan
        interestFactor = interestFactorForPaymentNo mode (interestRatePct loan) terms
        in calculatePaymentPrice (initialRemainingPrincipal loan) interestFactor (numberOfPrincipalPayments loan)

validateLoan ∷ LoanQuery → Either [LoanQueryValidationError] LoanQuery
validateLoan loan = let
    errors = catMaybes [
      checkPaymentStartDate
      , checkInvalidAmount
      , checkInvalidGracePeriod
      , checkInvalidInterestRate
      , checkInvalidPaymentNo
      , checkInvalidComission
      , checkPaymentFrequency
      ]
    checkPaymentStartDate
      | startDate loan > fromMaybe (startDate loan) (firstIncorporationEvent loan) = Just InvalidStartDate
      | otherwise = Nothing
    checkInvalidGracePeriod
      | loanTerms loan <= principalGracePeriodTerms loan  = Just InvalidGracePeriod
      | otherwise = Nothing
    checkInvalidInterestRate
      | interestRate loan <= 0  = Just InvalidInterestRate
      | otherwise = Nothing
    checkInvalidPaymentNo
      | loanTerms loan <= 0  = Just InvalidPaymentNo
      | otherwise = Nothing
    checkInvalidComission
      | totalCommissionAbsolute loan + totalCommissionPercent loan >= loanAmount loan = Just InvalidCommission
      | otherwise = Nothing
    checkInvalidAmount
      | loanAmount loan <= 1 = Just InvalidAmount
      | otherwise = Nothing
    checkPaymentFrequency
      | interestMode loan `elem` [ThreeSixtyDay, ThreeSixtyFiveDay, TwoFiveTwoDay]
          = (if paymentFrequency loan `notElem` [Biweekly, Weekly, Daily] then Just InvalidFrequency else Nothing)
      | otherwise = (if paymentFrequency loan `notElem` [Monthly, Yearly] then Just InvalidFrequency else Nothing)
  in if null errors then Right loan else Left errors

validateAndCalculate :: LoanQuery -> Either [LoanQueryValidationError] LoanQueryResult
validateAndCalculate loan = do
  validLoan <- validateLoan loan
  Right (calculate validLoan)

data PaymentItemType where
  InitialPayment :: LoanQuery -> PaymentItemType
  FirstIncorporationPayment :: LoanQuery -> PaymentItemType
  RegularPayment :: LoanQuery -> AmortizationTableItem -> PaymentItemType

paymentItem :: PaymentItemType -> AmortizationTableItem
paymentItem (InitialPayment loan) = AmortizationTableItem {
      dueDate               = startDate loan
      , paymentNo           = 0
      , accruedInterest     = 0
      , paymentAmortization = 0
      , remainingPrincipal  = loanAmount loan
      , dueAmount           = 0
      , financialTax        = 0
      }
paymentItem (FirstIncorporationPayment loan) = AmortizationTableItem {
      dueDate               = fromMaybe (error "firstIncorporationEvent") (firstIncorporationEvent loan)
      , paymentNo           = 1
      , accruedInterest     = initialRemainingPrincipal loan - loanAmount loan
      , paymentAmortization = 0
      , remainingPrincipal  = initialRemainingPrincipal loan
      , dueAmount           = 0
      , financialTax        = 0
      }
paymentItem (RegularPayment loan prevItem) = let
        paymentNo' = paymentNo prevItem + 1
        balance = remainingPrincipal prevItem
        payment = pmt loan paymentNo' balance
        interest = interestForPaymentNo loan paymentNo' balance
        dueDate' = currentDate loan paymentNo'
        amortization = max 0 (payment - interest)
        rmng = balance + interest - payment
    in AmortizationTableItem
      { dueDate              = dueDate'
      , dueAmount            = payment
      , paymentNo            = paymentNo'
      , accruedInterest      = interest
      , paymentAmortization  = max amortization 0
      , remainingPrincipal   = rmng
      , financialTax         = tax loan paymentNo' amortization
      }

calculate :: LoanQuery -> LoanQueryResult
calculate loan = flip evalState (loan, []) $ do
  forM_ [0..numberOfPayments loan] $ \paymentNo' -> do
    (q, s) <- get
    let
      paymentFor paymentNo''
        | paymentNo'' == 0                               = (q, s ++ [paymentItem $ InitialPayment loan])
        | paymentNo'' == 1 && hasIncorporationEvent loan = (q, s ++ [paymentItem $ FirstIncorporationPayment loan])
        | otherwise                                      = (q, s ++ [paymentItem $ RegularPayment loan (last s)])
      in put (paymentFor paymentNo')
  gets summary'

summary' :: (LoanQuery, [AmortizationTableItem]) -> LoanQueryResult
summary' (loan, items) = let
  accumulate field = foldr (\p acc -> acc + field p) 0 items
  table = AmortizationTable { paymentItems = items }
  payments = filter (\i -> dueAmount i > 0) items
  netValue' = netValue loan table
  cashflow = (startDate loan, -netValue') : map (\item -> (dueDate item, dueAmount item)) payments
  irr = fromMaybe 0 (calculateIrr cashflow)
  mirr = ((1 + irr) ** (1/12)) - 1
  in  LoanQueryResult
        { resultSchedule              = table
        , resultNetValue              = netValue'
        , resultTax                   = accumulate financialTax
        , resultStartPrincipal        = remainingPrincipal (head items)
        , resultFinalPrincipal        = remainingPrincipal (last items)
        , resultInterest              = accumulate accruedInterest
        , resultTotalPayment          = accumulate dueAmount
        , resultCommissions           = totalCommissionValue loan
        , resultYearlyEffectiveRate   = irr * 100
        , resultMonthlyEffectiveRate  = mirr * 100
        , resultYearlyNominalRate     = yearlyRate (interestMode loan) (interestRatePct loan) * 100
        , resultMonthlyNominalRate    = interestRate loan
        }

-- Calculate the total value of the given list of commissions
totalCommissionValue ∷ LoanQuery → Double
totalCommissionValue loanQuery = totalCommissionAbsolute loanQuery + totalCommissionPercent loanQuery

totalCommissionAbsolute ∷ LoanQuery → Double
totalCommissionAbsolute loanQuery =
  let absoluteValue (Commission Absolute value) = value
      absoluteValue _                           = 0.0
      commissionValues = map absoluteValue $ commissions loanQuery
  in sum commissionValues

totalCommissionPercent ∷ LoanQuery → Double
totalCommissionPercent loanQuery =
  let
      percentageValue (Commission Percentage value) = value
      percentageValue _                             = 0.0
      commissionValues = map (\c -> (percentageValue c / 100) * loanAmount loanQuery) $ commissions loanQuery
  in sum commissionValues

-- Convert a LoanQuery into another where the netValue of the later is equal to the amount of the first
asNetValueLoan ∷ LoanQuery → Maybe LoanQuery
asNetValueLoan loan =
  let target = loanAmount loan
      netValueQuery x = target - netValue loan {loanAmount = x } (resultSchedule $ calculate loan {loanAmount = x })
      tolerance = 0.005
      maxIterations = 40
      res   = goalSeek netValueQuery tolerance target maxIterations
  in  case res of
    Nothing   -> Nothing
    Just res' -> Just $ loan { loanAmount = res' }

formatNumber ∷ Double → String
formatNumber = printf "%10.2f"

formatCurrency ∷ Double → String
formatCurrency num = "R$ " ++ formatNumber num

formatPercent ∷ Double → String
formatPercent num  = " " ++ printf "%10.6f" num ++ " %"

instance Show LoanQueryResult where
  show result =
    let table = show (resultSchedule result)
    in  table
          ++ "\n "
          ++ "\nInitialAmount:  "
          ++ formatCurrency (resultStartPrincipal result)
          ++ "\nFinalAmount:    "
          ++ formatCurrency (resultFinalPrincipal result)          
          ++ "\nIRR:            "
          ++ formatPercent (resultYearlyEffectiveRate result)
          ++ "\nMonthly IRR:    "
          ++ formatPercent (resultMonthlyEffectiveRate result)
          ++ "\nTax:            "
          ++ formatCurrency (resultTax result)
          ++ "\nCommissions:    "
          ++ formatCurrency (resultCommissions result)
          ++ "\nNet:            "
          ++ formatCurrency (resultNetValue result)
          ++ "\nInterest:       "
          ++ formatCurrency (resultInterest result)
          ++ "\nTotal PMT:      "
          ++ formatCurrency (resultTotalPayment result)
          ++ "\n"

instance Show AmortizationTable where
  show amortizationTable' =
    let
      generateRow (AmortizationTableItem date payment interest principal remaining total tax')
        = let paymentStr   = printf "%3d" payment
              dateStr      = formatDate date
              interestStr  = formatCurrency interest
              principalStr = formatCurrency principal
              remainingStr = formatCurrency remaining
              totalStr     = formatCurrency total
              taxStr       = formatCurrency tax'
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
