{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module LoanApi
    ( HasbanAPI
    , hasbanServer
    ) where

import           Data.Aeson
import           GHC.Generics   (Generic)
import           Servant

-- Hasban imports
import           Hasban (PersonalLoan(..), CreditCard(..), Money(..), Rate(..),
                        AmortizationType(..), MinPayment(..), Payment(..),
                        CreditCardStatement(..), generateSchedule, monthlyPayment,
                        totalInterest, analyzeBalance, validatePersonalLoan,
                        Fee(..), FeeType(..), GracePeriod(..),
                        totalFees, netDisbursement, findPrincipalForNetValue)
import           Date (Frequency(..), toDate)

-- ============================================
-- API RESPONSE WRAPPER
-- ============================================

-- | Generic API response wrapper for clean JSON output
-- Success: {"success": true, "data": {...}}
-- Error: {"success": false, "error": "message"}
data ApiResponse a
    = ApiSuccess a
    | ApiError String
    deriving (Show, Generic)

instance ToJSON a => ToJSON (ApiResponse a) where
    toJSON (ApiSuccess a) = object
        [ "success" .= True
        , "data" .= a
        ]
    toJSON (ApiError msg) = object
        [ "success" .= False
        , "error" .= msg
        ]

instance FromJSON a => FromJSON (ApiResponse a) where
    parseJSON = withObject "ApiResponse" $ \v -> do
        success <- v .: "success"
        if success
            then ApiSuccess <$> v .: "data"
            else ApiError <$> v .: "error"

-- Helper to convert Either to ApiResponse
eitherToApi :: Either String a -> ApiResponse a
eitherToApi (Left err) = ApiError err
eitherToApi (Right val) = ApiSuccess val

-- ============================================
-- REQUEST/RESPONSE TYPES
-- ============================================

-- | Request for personal loan calculation
data PersonalLoanRequest = PersonalLoanRequest
    { plrPrincipal    :: Double
    , plrAnnualRate   :: Double   -- As decimal, e.g., 0.08 for 8%
    , plrTerms        :: Int
    , plrStartYear    :: Integer
    , plrStartMonth   :: Int
    , plrStartDay     :: Int
    , plrAmortization :: String   -- "price", "sac", or "bullet"
    , plrFrequency    :: String   -- "monthly", "biweekly", "weekly", etc.
    , plrFees         :: Maybe [FeeRequest]       -- Optional fees
    , plrGracePeriod  :: Maybe GracePeriodRequest -- Optional grace period
    } deriving (Show, Generic)

instance FromJSON PersonalLoanRequest where
    parseJSON = withObject "PersonalLoanRequest" $ \v -> PersonalLoanRequest
        <$> v .: "principal"
        <*> v .: "annual_rate"
        <*> v .: "terms"
        <*> v .: "start_year"
        <*> v .: "start_month"
        <*> v .: "start_day"
        <*> v .: "amortization"
        <*> v .: "frequency"
        <*> v .:? "fees"
        <*> v .:? "grace_period"

instance ToJSON PersonalLoanRequest where
    toJSON r = object
        [ "principal" .= plrPrincipal r
        , "annual_rate" .= plrAnnualRate r
        , "terms" .= plrTerms r
        , "start_year" .= plrStartYear r
        , "start_month" .= plrStartMonth r
        , "start_day" .= plrStartDay r
        , "amortization" .= plrAmortization r
        , "frequency" .= plrFrequency r
        , "fees" .= plrFees r
        , "grace_period" .= plrGracePeriod r
        ]

-- | Fee request
data FeeRequest = FeeRequest
    { feeType  :: String  -- "fixed" or "percentage"
    , feeValue :: Double
    } deriving (Show, Generic)

instance FromJSON FeeRequest where
    parseJSON = withObject "FeeRequest" $ \v -> FeeRequest
        <$> v .: "type"
        <*> v .: "value"

instance ToJSON FeeRequest where
    toJSON f = object
        [ "type" .= feeType f
        , "value" .= feeValue f
        ]

-- | Grace period request
data GracePeriodRequest = GracePeriodRequest
    { gpTermsReq     :: Int   -- Number of interest-only payments
    , gpCapitalizeReq :: Bool  -- Whether to capitalize interest
    } deriving (Show, Generic)

instance FromJSON GracePeriodRequest where
    parseJSON = withObject "GracePeriodRequest" $ \v -> GracePeriodRequest
        <$> v .: "terms"
        <*> v .: "capitalize"

instance ToJSON GracePeriodRequest where
    toJSON g = object
        [ "terms" .= gpTermsReq g
        , "capitalize" .= gpCapitalizeReq g
        ]

-- | Response for personal loan calculation
data PersonalLoanResponse = PersonalLoanResponse
    { plresMonthlyPayment  :: Double
    , plresTotalInterest   :: Double
    , plresTotalPayments   :: Double
    , plresTotalFees       :: Double
    , plresNetDisbursement :: Double
    , plresSchedule        :: [PaymentResponse]
    } deriving (Show, Generic)

instance FromJSON PersonalLoanResponse where
    parseJSON = withObject "PersonalLoanResponse" $ \v -> PersonalLoanResponse
        <$> v .: "monthly_payment"
        <*> v .: "total_interest"
        <*> v .: "total_payments"
        <*> v .: "total_fees"
        <*> v .: "net_disbursement"
        <*> v .: "schedule"

instance ToJSON PersonalLoanResponse where
    toJSON r = object
        [ "monthly_payment" .= plresMonthlyPayment r
        , "total_interest" .= plresTotalInterest r
        , "total_payments" .= plresTotalPayments r
        , "total_fees" .= plresTotalFees r
        , "net_disbursement" .= plresNetDisbursement r
        , "schedule" .= plresSchedule r
        ]

-- | Payment in schedule
data PaymentResponse = PaymentResponse
    { prNumber    :: Int
    , prInterest  :: Double
    , prPrincipal :: Double
    , prTotal     :: Double
    , prBalance   :: Double
    , prTax       :: Maybe Double
    } deriving (Show, Generic)

instance FromJSON PaymentResponse where
    parseJSON = withObject "PaymentResponse" $ \v -> PaymentResponse
        <$> v .: "number"
        <*> v .: "interest"
        <*> v .: "principal"
        <*> v .: "total"
        <*> v .: "balance"
        <*> v .:? "tax"

instance ToJSON PaymentResponse where
    toJSON p = object
        [ "number" .= prNumber p
        , "interest" .= prInterest p
        , "principal" .= prPrincipal p
        , "total" .= prTotal p
        , "balance" .= prBalance p
        , "tax" .= prTax p
        ]

-- | Credit card analysis request
data CreditCardRequest = CreditCardRequest
    { ccrBalance    :: Double
    , ccrAnnualRate :: Double   -- APR as decimal
    , ccrMinType    :: String   -- "fixed", "percent", or "greater_of"
    , ccrMinFixed   :: Maybe Double
    , ccrMinPercent :: Maybe Double
    } deriving (Show, Generic)

instance FromJSON CreditCardRequest where
    parseJSON = withObject "CreditCardRequest" $ \v -> CreditCardRequest
        <$> v .: "balance"
        <*> v .: "annual_rate"
        <*> v .: "min_type"
        <*> v .:? "min_fixed"
        <*> v .:? "min_percent"

instance ToJSON CreditCardRequest where
    toJSON c = object
        [ "balance" .= ccrBalance c
        , "annual_rate" .= ccrAnnualRate c
        , "min_type" .= ccrMinType c
        , "min_fixed" .= ccrMinFixed c
        , "min_percent" .= ccrMinPercent c
        ]

-- | Credit card analysis response
data CreditCardResponse = CreditCardResponse
    { ccresBalance       :: Double
    , ccresMinPayment    :: Double
    , ccresMonthlyRate   :: Double
    , ccresPayoffMonths  :: Maybe Int
    , ccresTotalInterest :: Double
    } deriving (Show, Generic)

instance FromJSON CreditCardResponse where
    parseJSON = withObject "CreditCardResponse" $ \v -> CreditCardResponse
        <$> v .: "balance"
        <*> v .: "min_payment"
        <*> v .: "monthly_rate"
        <*> v .:? "payoff_months"
        <*> v .: "total_interest"

instance ToJSON CreditCardResponse where
    toJSON c = object
        [ "balance" .= ccresBalance c
        , "min_payment" .= ccresMinPayment c
        , "monthly_rate" .= ccresMonthlyRate c
        , "payoff_months" .= ccresPayoffMonths c
        , "total_interest" .= ccresTotalInterest c
        ]

-- | Net value loan request (find principal for desired net disbursement)
data NetValueRequest = NetValueRequest
    { nvrDesiredNet   :: Double  -- Desired net disbursement
    , nvrAnnualRate   :: Double  -- Annual rate (decimal)
    , nvrTerms        :: Int     -- Number of terms
    , nvrFees         :: [FeeRequest]  -- Fees to apply
    } deriving (Show, Generic)

instance FromJSON NetValueRequest where
    parseJSON = withObject "NetValueRequest" $ \v -> NetValueRequest
        <$> v .: "desired_net"
        <*> v .: "annual_rate"
        <*> v .: "terms"
        <*> v .: "fees"

instance ToJSON NetValueRequest where
    toJSON n = object
        [ "desired_net" .= nvrDesiredNet n
        , "annual_rate" .= nvrAnnualRate n
        , "terms" .= nvrTerms n
        , "fees" .= nvrFees n
        ]

-- | Net value loan response
data NetValueResponse = NetValueResponse
    { nvrPrincipal       :: Double  -- Required principal
    , nvrTotalFees       :: Double  -- Total fees
    , nvrNetDisbursement :: Double  -- Actual net disbursement (should match desired)
    } deriving (Show, Generic)

instance FromJSON NetValueResponse where
    parseJSON = withObject "NetValueResponse" $ \v -> NetValueResponse
        <$> v .: "principal"
        <*> v .: "total_fees"
        <*> v .: "net_disbursement"

instance ToJSON NetValueResponse where
    toJSON n = object
        [ "principal" .= nvrPrincipal n
        , "total_fees" .= nvrTotalFees n
        , "net_disbursement" .= nvrNetDisbursement n
        ]

-- ============================================
-- API DEFINITION
-- ============================================

-- | Hasban API type
type HasbanAPI = "hasban" :>
    (    "personal" :> ReqBody '[JSON] PersonalLoanRequest :> Post '[JSON] (ApiResponse PersonalLoanResponse)
    :<|> "creditcard" :> ReqBody '[JSON] CreditCardRequest :> Post '[JSON] (ApiResponse CreditCardResponse)
    :<|> "net" :> ReqBody '[JSON] NetValueRequest :> Post '[JSON] (ApiResponse NetValueResponse)
    )

-- ============================================
-- HANDLERS
-- ============================================

-- | Convert request to PersonalLoan
toPersonalLoan :: PersonalLoanRequest -> Either String PersonalLoan
toPersonalLoan req = do
    amort <- parseAmortization (plrAmortization req)
    freq <- parseFrequency (plrFrequency req)
    fees <- case plrFees req of
        Nothing -> Right []
        Just feeReqs -> mapM parseFee feeReqs
    let gracePeriod = case plrGracePeriod req of
            Nothing -> Nothing
            Just gp -> Just $ GracePeriod (gpTermsReq gp) (gpCapitalizeReq gp)
    let loan = PersonalLoan
            { plPrincipal = Money (plrPrincipal req)
            , plAnnualRate = Rate (plrAnnualRate req)
            , plTerms = plrTerms req
            , plStartDate = toDate (plrStartYear req, plrStartMonth req, plrStartDay req)
            , plAmortization = amort
            , plFrequency = freq
            , plFees = fees
            , plGracePeriod = gracePeriod
            }
    case validatePersonalLoan loan of
        Left errs -> Left $ "Validation errors: " ++ show errs
        Right _   -> Right loan

-- | Parse fee request to Fee type
parseFee :: FeeRequest -> Either String Fee
parseFee req = case feeType req of
    "fixed"      -> Right $ Fee FixedFee (Money (feeValue req))
    "percentage" -> Right $ Fee PercentageFee (Money (feeValue req))
    t            -> Left $ "Invalid fee type: " ++ t

parseAmortization :: String -> Either String AmortizationType
parseAmortization "price"  = Right Price
parseAmortization "sac"    = Right Sac
parseAmortization "bullet" = Right Bullet
parseAmortization s        = Left $ "Invalid amortization type: " ++ s

parseFrequency :: String -> Either String Frequency
parseFrequency "monthly"  = Right Monthly
parseFrequency "biweekly" = Right Biweekly
parseFrequency "weekly"   = Right Weekly
parseFrequency "daily"    = Right Daily
parseFrequency "yearly"   = Right Yearly
parseFrequency s          = Left $ "Invalid frequency: " ++ s

-- | Handle personal loan request
handlePersonalLoan :: PersonalLoanRequest -> Either String PersonalLoanResponse
handlePersonalLoan req = do
    loan <- toPersonalLoan req
    let schedule = generateSchedule loan
        pmt = monthlyPayment loan
        interest = totalInterest loan
        fees = totalFees loan
        netDisb = netDisbursement loan
        scheduleResp = map paymentToResponse schedule
    Right PersonalLoanResponse
        { plresMonthlyPayment = unMoney pmt
        , plresTotalInterest = unMoney interest
        , plresTotalPayments = unMoney pmt * fromIntegral (plrTerms req)
        , plresTotalFees = unMoney fees
        , plresNetDisbursement = unMoney netDisb
        , plresSchedule = scheduleResp
        }

paymentToResponse :: Payment -> PaymentResponse
paymentToResponse p = PaymentResponse
    { prNumber = paymentNumber p
    , prInterest = unMoney (paymentInterest p)
    , prPrincipal = unMoney (paymentPrincipal p)
    , prTotal = unMoney (paymentTotal p)
    , prBalance = unMoney (paymentBalance p)
    , prTax = fmap unMoney (paymentTax p)
    }

-- | Convert request to CreditCard
toCreditCard :: CreditCardRequest -> Either String CreditCard
toCreditCard req = do
    minPmt <- parseMinPayment (ccrMinType req) (ccrMinFixed req) (ccrMinPercent req)
    Right CreditCard
        { ccLimit = Money 0  -- Not used in analysis
        , ccAnnualRate = Rate (ccrAnnualRate req)
        , ccMinPayment = minPmt
        }

parseMinPayment :: String -> Maybe Double -> Maybe Double -> Either String MinPayment
parseMinPayment "fixed" (Just fixed) _ = Right $ FixedAmount (Money fixed)
parseMinPayment "percent" _ (Just pct) = Right $ PercentOfBalance (Rate pct)
parseMinPayment "greater_of" (Just fixed) (Just pct) = Right $ GreaterOf (Money fixed) (Rate pct)
parseMinPayment t f p = Left $ "Invalid min payment: type=" ++ t ++ " fixed=" ++ show f ++ " pct=" ++ show p

-- | Handle credit card request
handleCreditCard :: CreditCardRequest -> Either String CreditCardResponse
handleCreditCard req = do
    card <- toCreditCard req
    let balance = Money (ccrBalance req)
        stmt = analyzeBalance card balance
    Right CreditCardResponse
        { ccresBalance = ccrBalance req
        , ccresMinPayment = unMoney (stmtMinPayment stmt)
        , ccresMonthlyRate = unRate (stmtInterestRate stmt)
        , ccresPayoffMonths = stmtPayoffMonths stmt
        , ccresTotalInterest = unMoney (stmtTotalInterest stmt)
        }

-- | Handle net value loan request (find principal for desired net disbursement)
handleNetValue :: NetValueRequest -> Either String NetValueResponse
handleNetValue req = do
    fees <- mapM parseFee (nvrFees req)
    case findPrincipalForNetValue
           (Money (nvrDesiredNet req))
           (Rate (nvrAnnualRate req))
           (nvrTerms req)
           fees of
        Nothing -> Left "Cannot calculate principal: percentage fees >= 100%"
        Just (Money principal) ->
            let totalFeesAmt = sum [calcFeeAmount fee principal | fee <- fees]
            in Right NetValueResponse
                { nvrPrincipal = principal
                , nvrTotalFees = totalFeesAmt
                , nvrNetDisbursement = principal - totalFeesAmt
                }
  where
    calcFeeAmount :: Fee -> Double -> Double
    calcFeeAmount (Fee FixedFee (Money amt)) _ = amt
    calcFeeAmount (Fee PercentageFee (Money pct)) principal = principal * pct

-- | Hasban server
hasbanServer :: Server HasbanAPI
hasbanServer = personal :<|> creditcard :<|> netValue
  where
    personal req = return (eitherToApi $ handlePersonalLoan req)
    creditcard req = return (eitherToApi $ handleCreditCard req)
    netValue req = return (eitherToApi $ handleNetValue req)
