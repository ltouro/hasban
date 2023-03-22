{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use isJust" #-}

module LoanCalculator
  ( LoanQuery(..)
  , LoanQueryValidationError
  , Frequency(..)
  , Commission(..)
  , CommissionType(..)
  , AmortizationType(..)
  , queryLoan
  , queryLoan'
  , asNetValueLoan
  , LoanQueryResult(..)
  , AmortizationTable(..)
  , AmortizationTableItem(..)
  , InterestMode(..)
  ) where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Maybe             (catMaybes, fromMaybe)
import           Date                   (Date (..), Frequency (..), addTerm,
                                         formatDate, numberOfDays, numberOfWeekdays, previousMonthAniversary, day, numberOfMonths)
import           GHC.Generics           (Generic)
import           GoalSeek               (goalSeek)
import           Text.PrettyPrint.Boxes (hsep, left, render, text, vsep)
import           Text.Printf            (printf)
import InterestFactor (interestFactor252MonthsIrregular, interestFactor252BDays, interestRate36XMonthsIrregular, DaysInYear (Days360, Days365), interestRate36XDays, interestUnitValue, interestRate36XMonths, interestFactor252Months21)

data LoanQuery = LoanQuery
  { loanAmount                :: CurrencyAmount
  , interestRate              :: InterestRate
  , interestMode              :: InterestMode
  , loanTerms                 :: PaymentNo
  , paymentFrequency          :: Frequency
  , startDate                 :: Date
  , firstIncorporationEvent   :: Maybe Date
  , principalGracePeriodTerms :: PaymentNo
  , commissions               :: [Commission]
  , amortizationType          :: AmortizationType
  }
  deriving (Show, Generic)
instance FromJSON LoanQuery
instance ToJSON LoanQuery

isRegularMode :: LoanQuery -> Bool
isRegularMode loan = case firstIncorporationEvent loan of
    Nothing -> True
    Just d -> day d == day (startDate loan)

currentDate :: LoanQuery -> PaymentNo -> Date
currentDate loan paymentNo'
  | paymentNo' == 0 = startDate loan
  | not (isRegularMode loan) && paymentNo' == 1 =
      fromMaybe (error "not expected") (firstIncorporationEvent loan)
  | otherwise = addTerm (currentDate loan $ paymentNo' - 1) (paymentFrequency loan) 1

numberOfPayments :: LoanQuery -> PaymentNo
numberOfPayments loan
  | firstIncorporationEvent loan /= Nothing = (loanTerms loan) + 1
  | otherwise = loanTerms loan

type PaymentNo = Int
type CurrencyAmount = Double
type InterestRate = Double

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
  { resultSchedule     :: AmortizationTable
  , resultNetValue     :: CurrencyAmount
  , resultTax          :: CurrencyAmount
  , resultPrincipal    :: CurrencyAmount
  , resultInterest     :: CurrencyAmount
  , resultTotalPayment :: CurrencyAmount
  , resultCommissions  :: CurrencyAmount
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

data InterestMode =
  ThreeSixtyFiveMonth  -- 365 dias, juros mensal
  | ThreeSixtyFiveDay  -- 365 dias, juros diário
  | ThreeSixtyMonth    -- 365 dias, juros diário
  | ThreeSixtyDay      -- 360 dias, juros diário
  | TwoFiveTwoMonth    -- 252 dias (úteis), juros mensal
  | TwoFiveTwoDay      -- 252 dias (úteis), juros diário
  deriving (Show, Generic)
instance FromJSON InterestMode
instance ToJSON InterestMode

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
  deriving (Show, Generic)
instance FromJSON LoanQueryValidationError
instance ToJSON LoanQueryValidationError

-- FV = PV * (1 + CR)
futureValue ∷ CurrencyAmount → InterestRate → PaymentNo → CurrencyAmount
futureValue pv r n = pv * (1 + compositeRate r n)

-- CR = (1+ r)^n - 1
compositeRate ∷ InterestRate → PaymentNo → InterestRate
compositeRate r n = ((1 + r) ^ n) - 1

-- -- DR = (1+ r)^ 1 / n - 1
-- decomposeRate ∷ InterestRate → Double → InterestRate
-- decomposeRate r n = ( (1 + r) ** (1 / n) ) - 1

-- This function calculates the variable monthly payment for an SAC amortization schedule
-- given the loan amount, interest rate, loan term andd balance
calculatePaymentSac ∷ CurrencyAmount → InterestRate → PaymentNo → CurrencyAmount → Double
calculatePaymentSac loanAmount' interestRate' loanTerm balance =
  let amortization = loanAmount' / fromIntegral loanTerm
  in  amortization + (interestRate' / 100) * balance

-- This function calculates the fixed monthly payment for an PRICE amortization schedule
-- given the loan amount, interest rate, and loan term
calculatePaymentPrice ∷ CurrencyAmount → InterestRate → PaymentNo → Double
calculatePaymentPrice loanAmount' interestRate' loanTerm =
  let
    paymentNo' = fromIntegral loanTerm
    r = interestRate' / 100
  in  loanAmount'
        * futureValue r r paymentNo'
        / compositeRate interestRate' paymentNo'

-- Sums the financialTax of every AmortizationTableItem in table
sumTax ∷ AmortizationTable → Double
sumTax table = foldr (\p acc -> acc + financialTax p) 0 (paymentItems table)

-- Gets the initial remaninig amount and subtract taxes
netValue ∷ LoanQuery → AmortizationTable → Double
netValue loan table =
  remainingPrincipal (head (paymentItems table))
    - sumTax table
    - totalCommissionValue loan

validateLoan ∷ LoanQuery → Either [LoanQueryValidationError] LoanQuery
validateLoan loan = let
    errors = catMaybes [
      checkPaymentStartDate
      , checkInvalidAmount
      , checkInvalidGracePeriod
      , checkInvalidInterestRate
      , checkInvalidPaymentNo
      , checkInvalidComission
      ]
    checkPaymentStartDate
      | startDate loan < fromMaybe (startDate loan) (firstIncorporationEvent loan) = Just InvalidStartDate
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
      | totalCommissionAbsolute loan >= loanAmount loan = Just InvalidCommission
      | totalCommissionPercent loan >= 80 = Just InvalidCommission
      | otherwise = Nothing
    checkInvalidAmount
      | loanAmount loan <= 1 = Just InvalidAmount
      | otherwise = Nothing
  in if null errors then Right loan else Left errors

queryLoan' ∷ LoanQuery → Either [LoanQueryValidationError] LoanQueryResult
queryLoan' loan = case validateLoan loan of
  Right validLoan      -> Right (queryLoan validLoan)
  Left validationError -> Left validationError

queryLoan ∷ LoanQuery → LoanQueryResult
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

generateAmortizationTable ∷ LoanQuery → AmortizationTable
generateAmortizationTable loan = AmortizationTable
  { paymentItems = foldl itemsForPaymentNo [initialPaymentItem] [1 .. numberOfPayments loan] }
  where
    interestForPaymentNo :: PaymentNo -> CurrencyAmount -> CurrencyAmount
    interestForPaymentNo paymentNo' remainingPrincipal' =
      let
        mode = interestMode loan
        previousDate = currentDate loan (paymentNo' - 1)
        currentDate' = currentDate loan paymentNo'
        -- days = fromIntegral $ numberOfDays previousDate currentDate'
        months = numberOfMonths previousDate currentDate'
        dct = numberOfDays previousDate (addTerm previousDate (paymentFrequency loan) 1)
        dcp = numberOfDays previousDate currentDate'
      in case mode of
        ThreeSixtyMonth -> let
            interestFactor = if isRegularMode loan
              then interestRate36XMonths yearlyRate months dct dcp Days360
              else let
                  baseDateForInterest = addTerm currentDate' (paymentFrequency loan) (negate $ fromIntegral months)
                  dctProRateAnt = addTerm baseDateForInterest (paymentFrequency loan) (-1)
                  dctProRata = numberOfDays dctProRateAnt baseDateForInterest
                  dcpProRata = numberOfDays previousDate baseDateForInterest
                in interestRate36XMonthsIrregular yearlyRate dcpProRata dctProRata months dcp dct Days360
            in interestUnitValue remainingPrincipal' interestFactor
        _ -> error "not implemented"

    -- number of payments where amortization > 0
    numberOfPrincipalPayments = loanTerms loan - principalGracePeriodTerms loan

    -- remainingPrincipal at the fixed start date
    initialRemainingPrincipal
      | firstIncorporationEvent loan /= Nothing = loanAmount loan + interestForPaymentNo 1 (loanAmount loan)
      | otherwise = loanAmount loan

    -- the rate adjusted to the accrual frequency    
    accrualRate = interestRate loan
    yearlyRate = compositeRate (interestRate loan) 12

    -- Compute the tax for the given payment number in the loan schedule
    tax ∷ PaymentNo → CurrencyAmount → Double
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
    initialPaymentItem ∷ AmortizationTableItem
    initialPaymentItem = AmortizationTableItem {
      dueDate               = startDate loan
      , paymentNo           = 0
      , accruedInterest     = 0
      , paymentAmortization = 0
      , remainingPrincipal  = initialRemainingPrincipal
      , dueAmount           = 0
      , financialTax        = 0
      }

    itemsForPaymentNo prevItems paymentNo' =
      let
        prevItem = last prevItems
        -- currentPaymentNo = paymentNo prevItem + 1
        balance = remainingPrincipal prevItem
        payment = pmt paymentNo' balance
        interest = interestForPaymentNo paymentNo' balance
        dueDate' = currentDate loan paymentNo'
        amort = max 0 (payment - interest)
      in
      prevItems ++ [AmortizationTableItem
      { dueDate              = dueDate'
      , dueAmount            = payment
      , paymentNo            = paymentNo'
      , accruedInterest      = interest
      , paymentAmortization  = max amort 0
      , remainingPrincipal   = balance + interest - payment
      , financialTax         = tax paymentNo' amort
      }]

    -- Compute the payment for a given payment number in the schedule
    pmt ∷ PaymentNo → CurrencyAmount → Double
    pmt paymentNo' balance
      | paymentNo' <=  principalGracePeriodTerms loan = interestForPaymentNo paymentNo' balance
      | otherwise = payment
      where
        payment = case amortizationType loan of
          SAC -> calculatePaymentSac initialRemainingPrincipal accrualRate numberOfPrincipalPayments balance
          PRICE -> calculatePaymentPrice initialRemainingPrincipal accrualRate numberOfPrincipalPayments

-- Calculate the total value of the given list of commissions
totalCommissionValue ∷ LoanQuery → Double
totalCommissionValue loanQuery = totalCommissionAbsolute loanQuery + (totalCommissionPercent loanQuery / 100 * loanAmount loanQuery)

totalCommissionAbsolute ∷ LoanQuery → Double
totalCommissionAbsolute loanQuery =
  let absoluteValue (Commission Absolute value) = value
      absoluteValue _                           = 0.0
      percentageValue (Commission Percentage _) = 0.0
      percentageValue _                         = 0.0
      commissionValues = map (\c -> absoluteValue c + (percentageValue c / 100) * loanAmount loanQuery) $ commissions loanQuery
  in sum commissionValues

totalCommissionPercent ∷ LoanQuery → Double
totalCommissionPercent loanQuery =
  let absoluteValue (Commission Absolute _) = 0.0
      absoluteValue _                       = 0.0
      percentageValue (Commission Percentage value) = value
      percentageValue _                             = 0.0
      commissionValues = map (\c -> absoluteValue c + (percentageValue c / 100) * loanAmount loanQuery) $ commissions loanQuery
  in sum commissionValues

-- Convert a LoanQuery into another where the netValue of the later is equal to the amount of the first
asNetValueLoan ∷ LoanQuery → Maybe LoanQuery
asNetValueLoan loan =
  let target = loanAmount loan
      netValueQuery x = target - netValue loan {loanAmount = x } (resultSchedule $ queryLoan loan {loanAmount = x })
      tolerance = 0.49
      maxIterations = 300
      res   = goalSeek netValueQuery tolerance target maxIterations
  in  case res of
    Nothing   -> Nothing
    Just res' -> Just $ loan { loanAmount = res' }

formatNumber ∷ Double → String
formatNumber = printf "%10.2f"

formatCurrency ∷ Double → String
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
