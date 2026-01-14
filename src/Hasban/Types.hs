{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasban.Types (
  -- * Core Financial Types
  Money(..)
  , Rate(..)
  , mkMoney
  , mkRate

  -- * Personal Loans (Amortizing)
  , PersonalLoan(..)
  , AmortizationType(..)
  , Frequency(..)
  , Payment(..)

  -- * Fees and Commissions
  , Fee(..)
  , FeeType(..)

  -- * Grace Periods
  , GracePeriod(..)

  -- * Credit Cards (Revolving)
  , CreditCard(..)
  , MinPayment(..)
  , CreditCardStatement(..)
  , PayoffMonth(..)
  , PayoffProjection(..)

  -- * Interest Calculation
  , InterestMethod(..)

  -- * Date re-export
  , Date(..)
  , toDate
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Date (Date(..), Frequency(..), toDate)

-- | Type-safe money wrapper (2 decimal precision for display)
newtype Money = Money { unMoney :: Double }
  deriving (Show, Eq, Ord, Num, Fractional, Generic)

instance FromJSON Money
instance ToJSON Money

-- | Smart constructor for Money - ensures positive values
mkMoney :: Double -> Maybe Money
mkMoney x
  | x >= 0    = Just (Money x)
  | otherwise = Nothing

-- | Type-safe percentage rate wrapper (stored as decimal, e.g., 0.08 for 8%)
newtype Rate = Rate { unRate :: Double }
  deriving (Show, Eq, Ord, Num, Fractional, Generic)

instance FromJSON Rate
instance ToJSON Rate

-- | Smart constructor for Rate - allows negative rates (rare but valid)
mkRate :: Double -> Rate
mkRate = Rate

-- ============================================
-- PERSONAL LOANS (Amortizing)
-- ============================================

-- | Personal loan configuration
data PersonalLoan = PersonalLoan
  { plPrincipal    :: Money         -- ^ Loan principal amount
  , plAnnualRate   :: Rate          -- ^ Annual interest rate (decimal)
  , plTerms        :: Int           -- ^ Number of payments
  , plStartDate    :: Date          -- ^ Loan start date
  , plAmortization :: AmortizationType  -- ^ Amortization method
  , plFrequency    :: Frequency     -- ^ Payment frequency
  , plFees         :: [Fee]         -- ^ Optional upfront fees (default: [])
  , plGracePeriod  :: Maybe GracePeriod  -- ^ Optional grace period (interest-only)
  } deriving (Show, Eq, Generic)

instance FromJSON PersonalLoan
instance ToJSON PersonalLoan

-- | Amortization types
data AmortizationType =
    Price   -- ^ Constant payment (PMT formula)
  | Sac     -- ^ Constant principal amortization
  | Bullet  -- ^ Interest only, principal at end
  deriving (Show, Eq, Generic)

instance FromJSON AmortizationType
instance ToJSON AmortizationType

-- ============================================
-- FEES AND COMMISSIONS
-- ============================================

-- | Fee (commission) that reduces net disbursement
data Fee = Fee FeeType Money
  deriving (Show, Eq, Generic)

instance FromJSON Fee
instance ToJSON Fee

-- | Fee type determines how the fee is calculated
data FeeType =
    FixedFee       -- ^ Fixed amount deducted from disbursement
  | PercentageFee  -- ^ Percentage of principal
  deriving (Show, Eq, Generic)

instance FromJSON FeeType
instance ToJSON FeeType

-- ============================================
-- GRACE PERIODS
-- ============================================

-- | Grace period configuration for interest-only payments
data GracePeriod = GracePeriod
  { gpTerms      :: Int   -- ^ Number of interest-only payments
  , gpCapitalize :: Bool  -- ^ Whether to add accrued interest to principal after grace period
  } deriving (Show, Eq, Generic)

instance FromJSON GracePeriod
instance ToJSON GracePeriod

-- | Single payment in amortization schedule
data Payment = Payment
  { paymentNumber    :: Int          -- ^ Payment sequence number
  , paymentDate      :: Date         -- ^ Payment due date
  , paymentInterest  :: Money        -- ^ Interest portion
  , paymentPrincipal :: Money        -- ^ Principal portion
  , paymentTotal     :: Money        -- ^ Total payment amount
  , paymentBalance   :: Money        -- ^ Remaining balance after payment
  , paymentTax       :: Maybe Money  -- ^ Optional tax (e.g., IOF) for this payment
  } deriving (Show, Eq, Generic)

instance FromJSON Payment
instance ToJSON Payment

-- ============================================
-- CREDIT CARDS (Revolving)
-- ============================================

-- | Credit card configuration
data CreditCard = CreditCard
  { ccLimit        :: Money         -- ^ Credit limit
  , ccAnnualRate   :: Rate          -- ^ Annual percentage rate (APR)
  , ccMinPayment   :: MinPayment    -- ^ Minimum payment rule
  } deriving (Show, Eq, Generic)

instance FromJSON CreditCard
instance ToJSON CreditCard

-- | Minimum payment calculation methods
data MinPayment
  = FixedAmount Money               -- ^ Fixed minimum (e.g., $25)
  | PercentOfBalance Rate           -- ^ Percentage of balance (e.g., 2%)
  | GreaterOf Money Rate            -- ^ Greater of fixed or percentage
  deriving (Show, Eq, Generic)

instance FromJSON MinPayment
instance ToJSON MinPayment

-- | Credit card statement analysis
data CreditCardStatement = CreditCardStatement
  { stmtBalance       :: Money      -- ^ Current balance
  , stmtMinPayment    :: Money      -- ^ Calculated minimum payment
  , stmtInterestRate  :: Rate       -- ^ Monthly interest rate
  , stmtPayoffMonths  :: Maybe Int  -- ^ Months to payoff at minimum
  , stmtTotalInterest :: Money      -- ^ Total interest if paying minimum
  } deriving (Show, Eq, Generic)

instance FromJSON CreditCardStatement
instance ToJSON CreditCardStatement

-- | Single month in payoff simulation
data PayoffMonth = PayoffMonth
  { pmMonth        :: Int     -- ^ Month number
  , pmPayment      :: Money   -- ^ Payment made
  , pmInterest     :: Money   -- ^ Interest charged
  , pmPrincipal    :: Money   -- ^ Principal paid
  , pmBalance      :: Money   -- ^ Remaining balance
  } deriving (Show, Eq, Generic)

instance FromJSON PayoffMonth
instance ToJSON PayoffMonth

-- | Payoff projection summary
data PayoffProjection = PayoffProjection
  { ppMonths        :: Int    -- ^ Total months to payoff
  , ppTotalPaid     :: Money  -- ^ Total amount paid
  , ppTotalInterest :: Money  -- ^ Total interest paid
  } deriving (Show, Eq, Generic)

instance FromJSON PayoffProjection
instance ToJSON PayoffProjection

-- ============================================
-- GENERIC INTEREST
-- ============================================

-- | Interest calculation method
data InterestMethod =
    SimpleInterest    -- ^ Linear interest: P * r * t
  | CompoundInterest  -- ^ Compound interest: P * (1 + r)^t - P
  deriving (Show, Eq, Generic)

instance FromJSON InterestMethod
instance ToJSON InterestMethod
