module Main where

import           Test.Hspec

import           Date (Date(..), Frequency(..), daysInMonth)
import           Test.QuickCheck (Arbitrary(arbitrary), choose)

-- Import CETIP21 test modules
import qualified Precision.CETIP21Spec as CETIP21Precision
import qualified InterestCalculation.MethodsSpec as Methods
import qualified Calendar.BusinessDaySpec as BusinessDay
import qualified ProRata.DelayedCorrectionSpec as ProRata
import qualified Reference.CETIP21ReferenceSpec as Reference

-- Import Hasban API test modules
import qualified Hasban.LoanSpec as HasbanLoan
import qualified Hasban.CreditCardSpec as HasbanCreditCard

main :: IO ()
main = hspec $ do
  -- Hasban API tests (core functionality)
  describe "Hasban API Tests" $ do
    HasbanLoan.spec
    HasbanCreditCard.spec

  -- CETIP21 compliance tests (Brazilian regulatory)
  describe "CETIP21 Compliance Tests" $ do
    CETIP21Precision.spec
    Methods.spec
    BusinessDay.spec
    ProRata.spec
    Reference.spec

-- Arbitrary instance for Date (used by other test modules)
instance Arbitrary Date where
  arbitrary = do
    year <- choose (2000, 2050)
    month <- choose (1, 12)
    day <- choose (1, daysInMonth year month)
    return $ Date (fromIntegral year) month day
