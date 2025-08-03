{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Domain.AmortizationSchedule
  ( AmortizationSchedule(..)
  , PaymentItem(..)
  , AmortizationResult(..)
  , calculateAmortizationSchedule
  , calculatePayment
  , calculateNetValue
  , calculateInternalRateOfReturn
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

import Domain.Types
import Domain.Loan (Loan(..), isGracePeriod, getInterestPeriod)
import Domain.BrazilianTax (totalIOFForAmortization)
import Infrastructure.DateCalculator (FinancialDate, addPaymentPeriods, daysBetween, monthsBetween, formatFinancialDate)
import Infrastructure.InterestCalculator (InterestFactor(..), calculateInterestFactor, interestForPeriod, roundToDecimals)
import IRR (calculateIrr)

-- | Individual payment item in the amortization schedule
data PaymentItem = PaymentItem
  { dueDate             :: FinancialDate
  , paymentNumber       :: PaymentNumber
  , accruedInterest     :: CurrencyAmount
  , paymentAmortization :: CurrencyAmount
  , remainingPrincipal  :: CurrencyAmount
  , dueAmount           :: CurrencyAmount
  , taxAmount           :: TaxAmount
  } deriving (Eq, Show, Generic)

instance FromJSON PaymentItem
instance ToJSON PaymentItem

-- | Complete amortization schedule
newtype AmortizationSchedule = AmortizationSchedule
  { paymentItems :: [PaymentItem]
  } deriving (Eq, Show, Generic)

instance FromJSON AmortizationSchedule
instance ToJSON AmortizationSchedule

-- | Complete amortization calculation result
data AmortizationResult = AmortizationResult
  { schedule               :: AmortizationSchedule
  , netValue               :: CurrencyAmount
  , totalTax               :: TaxAmount
  , startPrincipal         :: CurrencyAmount
  , finalPrincipal         :: CurrencyAmount
  , totalInterest          :: CurrencyAmount
  , totalPayments          :: CurrencyAmount
  , totalCommissions       :: CurrencyAmount
  , yearlyEffectiveRate    :: InterestRate
  , monthlyEffectiveRate   :: InterestRate
  , monthlyNominalRate     :: InterestRate
  , yearlyNominalRate      :: InterestRate
  } deriving (Eq, Show, Generic)

instance FromJSON AmortizationResult
instance ToJSON AmortizationResult

-- | Calculate complete amortization schedule for a loan
calculateAmortizationSchedule :: Loan -> AmortizationResult
calculateAmortizationSchedule loan = 
  let items = generatePaymentItems loan
      schedule = AmortizationSchedule items
  in buildAmortizationResult loan schedule

-- | Generate all payment items for the loan
generatePaymentItems :: Loan -> [PaymentItem]
generatePaymentItems loan = 
  let totalPayments = getTotalPaymentCount loan
      initialItem = createInitialPaymentItem loan
      incorporationItem = createIncorporationPaymentItem loan
      regularItems = createRegularPaymentItems loan (getLastPaymentItem loan [initialItem] incorporationItem)
  in case firstIncorporationEvent loan of
    Nothing -> initialItem : regularItems
    Just _ -> [initialItem, incorporationItem] ++ regularItems

-- | Create initial payment item (loan origination)
createInitialPaymentItem :: Loan -> PaymentItem
createInitialPaymentItem loan = PaymentItem
  { dueDate = startDate loan
  , paymentNumber = 0
  , accruedInterest = 0
  , paymentAmortization = 0
  , remainingPrincipal = loanAmount loan
  , dueAmount = 0
  , taxAmount = TaxAmount 0
  }

-- | Create incorporation event payment item (if applicable)
createIncorporationPaymentItem :: Loan -> PaymentItem
createIncorporationPaymentItem loan = 
  case firstIncorporationEvent loan of
    Nothing -> error "No incorporation event defined"
    Just eventDate -> 
      let initialPrincipal = loanAmount loan
          interestAccrued = calculateInterestForPayment loan 1 initialPrincipal
          newPrincipal = initialPrincipal + interestAccrued
      in PaymentItem
        { dueDate = eventDate
        , paymentNumber = 1
        , accruedInterest = interestAccrued
        , paymentAmortization = 0
        , remainingPrincipal = newPrincipal
        , dueAmount = 0
        , taxAmount = TaxAmount 0
        }

-- | Create regular payment items
createRegularPaymentItems :: Loan -> PaymentItem -> [PaymentItem]
createRegularPaymentItems loan lastItem =
  let startPaymentNo = unPaymentNumber (paymentNumber lastItem) + 1
      endPaymentNo = unPaymentNumber $ getTotalPaymentCount loan
  in map (createRegularPaymentItem loan) [startPaymentNo..endPaymentNo]
  where
    createRegularPaymentItem :: Loan -> Int -> PaymentItem
    createRegularPaymentItem loan' paymentNo =
      let prevItem = getPaymentItem loan' (PaymentNumber (paymentNo - 1))
          currentBalance = remainingPrincipal prevItem
          payment = calculatePayment loan' (PaymentNumber paymentNo) currentBalance
          interest = calculateInterestForPayment loan' (PaymentNumber paymentNo) currentBalance
          paymentDate = calculatePaymentDate loan' (PaymentNumber paymentNo)
          amortization = max 0 (payment - interest)
          newBalance = currentBalance + interest - payment
          tax = calculateTaxForPayment loan' (PaymentNumber paymentNo) amortization
      in PaymentItem
        { dueDate = paymentDate
        , paymentNumber = PaymentNumber paymentNo
        , accruedInterest = interest
        , paymentAmortization = amortization
        , remainingPrincipal = newBalance
        , dueAmount = payment
        , taxAmount = tax
        }

-- | Calculate payment amount for a specific payment number
calculatePayment :: Loan -> PaymentNumber -> CurrencyAmount -> CurrencyAmount
calculatePayment loan paymentNo balance
  | isGracePeriod loan paymentNo = calculateInterestForPayment loan paymentNo balance
  | otherwise = 
      let interest = calculateInterestForPayment loan paymentNo balance
          fixedPayment = calculateFixedPayment loan
      in case amortizationType loan of
        SAC -> fixedPayment + interest
        PRICE -> fixedPayment

-- | Calculate fixed payment amount based on amortization type
calculateFixedPayment :: Loan -> CurrencyAmount
calculateFixedPayment loan =
  let principalPayments = getNumberOfPrincipalPayments loan
      initialPrincipal = getInitialRemainingPrincipal loan
  in case amortizationType loan of
    SAC -> CurrencyAmount (unCurrencyAmount initialPrincipal / fromIntegral (unLoanTerms principalPayments))
    PRICE -> calculatePricePayment loan initialPrincipal principalPayments

-- | Calculate PRICE payment using present value formula
calculatePricePayment :: Loan -> CurrencyAmount -> LoanTerms -> CurrencyAmount
calculatePricePayment loan principal terms =
  let periods = getInitialPaymentCount loan
      factor = calculateInterestFactor (interestMode loan) (interestRate loan) (fromIntegral (unPaymentNumber periods))
      InterestFactor factorValue = factor
      rate = factorValue
      termCount = fromIntegral (unLoanTerms terms)
      principalValue = unCurrencyAmount principal
      payment = principalValue * (rate * ((1 + rate) ** termCount)) / (((1 + rate) ** termCount) - 1)
  in CurrencyAmount payment

-- | Calculate interest for a specific payment
calculateInterestForPayment :: Loan -> PaymentNumber -> CurrencyAmount -> CurrencyAmount
calculateInterestForPayment loan paymentNo principal =
  let periods = getNumberOfTermsForPayment loan paymentNo
      factor = calculateInterestFactor (interestMode loan) (interestRate loan) periods
  in interestForPeriod principal factor

-- | Calculate tax for a payment
calculateTaxForPayment :: Loan -> PaymentNumber -> CurrencyAmount -> TaxAmount
calculateTaxForPayment loan paymentNo amortization =
  let startDate' = startDate loan
      dueDate' = calculatePaymentDate loan paymentNo
  in totalIOFForAmortization (personType loan) amortization startDate' dueDate'

-- | Calculate payment date for a payment number
calculatePaymentDate :: Loan -> PaymentNumber -> FinancialDate
calculatePaymentDate loan paymentNo
  | unPaymentNumber paymentNo == 0 = startDate loan
  | unPaymentNumber paymentNo == 1 && hasIncorporationEvent loan = 
      fromMaybe (startDate loan) (firstIncorporationEvent loan)
  | otherwise = 
      let prevDate = calculatePaymentDate loan (PaymentNumber (unPaymentNumber paymentNo - 1))
          schedule = paymentSchedule loan
      in addPaymentPeriods prevDate schedule 1

-- Helper functions
getTotalPaymentCount :: Loan -> PaymentNumber
getTotalPaymentCount loan = 
  case firstIncorporationEvent loan of
    Nothing -> loanTerms loan
    Just _ -> PaymentNumber (unLoanTerms (loanTerms loan) + 1)

getNumberOfPrincipalPayments :: Loan -> LoanTerms
getNumberOfPrincipalPayments loan = 
  LoanTerms (unLoanTerms (loanTerms loan) - unPaymentNumber (principalGracePeriodTerms loan))

getInitialRemainingPrincipal :: Loan -> CurrencyAmount
getInitialRemainingPrincipal loan =
  case firstIncorporationEvent loan of
    Nothing -> loanAmount loan
    Just _ -> 
      let interest = calculateInterestForPayment loan 1 (loanAmount loan)
      in loanAmount loan + interest

getInitialPaymentCount :: Loan -> PaymentNumber
getInitialPaymentCount loan = if hasIncorporationEvent loan then 2 else 1

hasIncorporationEvent :: Loan -> Bool
hasIncorporationEvent loan = case firstIncorporationEvent loan of
  Just _ -> True
  Nothing -> False

getNumberOfTermsForPayment :: Loan -> PaymentNumber -> Integer
getNumberOfTermsForPayment loan paymentNo =
  let prevDate = calculatePaymentDate loan (PaymentNumber (unPaymentNumber paymentNo - 1))
      currentDate = calculatePaymentDate loan paymentNo
  in case getInterestPeriod loan paymentNo of
    Day -> daysBetween prevDate currentDate
    Month -> min 1 (monthsBetween prevDate currentDate)

getLastPaymentItem :: Loan -> [PaymentItem] -> PaymentItem -> PaymentItem
getLastPaymentItem loan items incorporationItem =
  case firstIncorporationEvent loan of
    Nothing -> last items
    Just _ -> incorporationItem

getPaymentItem :: Loan -> PaymentNumber -> PaymentItem
getPaymentItem loan paymentNo = 
  -- This is a simplified version - in practice, you'd need to build up the schedule
  error "getPaymentItem needs full schedule context"

-- | Build the complete amortization result
buildAmortizationResult :: Loan -> AmortizationSchedule -> AmortizationResult
buildAmortizationResult loan schedule =
  let items = paymentItems schedule
      paymentsOnly = filter (\item -> unCurrencyAmount (dueAmount item) > 0) items
      totalTax' = sumField (unTaxAmount . taxAmount) items
      totalInterest' = sumField (unCurrencyAmount . accruedInterest) items
      totalPayments' = sumField (unCurrencyAmount . dueAmount) items
      totalCommissions' = calculateTotalCommissions loan
      netValue' = calculateNetValue loan schedule
      startPrincipal' = if null items then 0 else remainingPrincipal (head items)
      finalPrincipal' = if null items then 0 else remainingPrincipal (last items)
      
      -- IRR calculation
      cashflow = (startDate loan, negate (unCurrencyAmount netValue')) : 
                 map (\item -> (dueDate item, unCurrencyAmount (dueAmount item))) paymentsOnly
      irr = fromMaybe 0 (calculateIrr $ map (\(date, amount) -> (convertToOldDate date, amount)) cashflow)
      monthlyIrr = ((1 + irr) ** (1/12)) - 1
      
  in AmortizationResult
    { schedule = schedule
    , netValue = netValue'
    , totalTax = TaxAmount totalTax'
    , startPrincipal = startPrincipal'
    , finalPrincipal = finalPrincipal'
    , totalInterest = CurrencyAmount totalInterest'
    , totalPayments = CurrencyAmount totalPayments'
    , totalCommissions = totalCommissions'
    , yearlyEffectiveRate = InterestRate (irr * 100)
    , monthlyEffectiveRate = InterestRate (monthlyIrr * 100)
    , monthlyNominalRate = interestRate loan
    , yearlyNominalRate = InterestRate (unInterestRate (interestRate loan) * 12)
    }

-- | Calculate net value (loan amount minus taxes and commissions)
calculateNetValue :: Loan -> AmortizationSchedule -> CurrencyAmount
calculateNetValue loan schedule =
  let totalTax' = sumField (unTaxAmount . taxAmount) (paymentItems schedule)
      totalCommissions' = calculateTotalCommissions loan
      loanAmount' = loanAmount loan
  in CurrencyAmount (unCurrencyAmount loanAmount' - totalTax' - unCurrencyAmount totalCommissions')

-- | Calculate internal rate of return for the loan
calculateInternalRateOfReturn :: Loan -> AmortizationSchedule -> Maybe Double
calculateInternalRateOfReturn loan schedule =
  let items = paymentItems schedule
      paymentsOnly = filter (\item -> unCurrencyAmount (dueAmount item) > 0) items
      netValue' = calculateNetValue loan schedule
      cashflow = (startDate loan, negate (unCurrencyAmount netValue')) : 
                 map (\item -> (dueDate item, unCurrencyAmount (dueAmount item))) paymentsOnly
  in calculateIrr $ map (\(date, amount) -> (convertToOldDate date, amount)) cashflow

-- Helper functions
sumField :: (a -> Double) -> [a] -> Double
sumField f = sum . map f

calculateTotalCommissions :: Loan -> CurrencyAmount
calculateTotalCommissions loan =
  let absoluteTotal = sum [unCommissionValue (commissionValue c) | c <- commissions loan, commissionType c == Absolute]
      percentageTotal = sum [(unCommissionValue (commissionValue c) / 100) * unCurrencyAmount (loanAmount loan) 
                           | c <- commissions loan, commissionType c == Percentage]
  in CurrencyAmount (absoluteTotal + percentageTotal)

-- Temporary conversion function (until we fully migrate from the old Date type)
convertToOldDate :: FinancialDate -> Date.Date
convertToOldDate = error "convertToOldDate needs implementation"