{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module InterestFactor (
  InterestMode(..),
  InterestRate,
  interestUnitValue
  , interestFactorForPaymentNo
  , effectiveRate
  , DaysInYear(..)
  , round'
  , InterestPeriod(..)
  , yearlyRate
) where
import           Data.Aeson
import           GHC.Generics

type InterestRate = Double

type CurrencyAmount = Double
type PaymentNo = Integer
data InterestPeriod = Day | Month;
-- type BusinessDays = Integer
-- type Months = Integer
-- type Days = Integer
data DaysInYear = Days360 | Days365
  deriving (Show, Eq)

-- x : number you want rounded, n : number of decimal places you want...
round' :: Double -> Integer -> Double
round' num sg = (fromIntegral . round $ num * f) / f
    where f = 10^sg

-- Valor unitário dos juros acumulados no período, calculado com 8 (oito) casas decimais
interestUnitValue :: CurrencyAmount -> InterestRate -> CurrencyAmount
interestUnitValue nominalValue interestFactor = round' (nominalValue * interestFactor) 8

data InterestMode =
  ThreeSixtyFiveMonth  -- 365 dias, juros mensal
  | ThreeSixtyFiveDay  -- 365 dias, juros diário
  | ThreeSixtyMonth    -- 360 dias, juros mensal
  | ThreeSixtyDay      -- 360 dias, juros diário
  | TwoFiveTwoMonth    -- 252 dias (úteis), juros mensal
  | TwoFiveTwoDay      -- 252 dias (úteis), juros diário
  deriving (Show, Generic, Eq)
instance FromJSON InterestMode
instance ToJSON InterestMode

interestFactorForPaymentNo :: InterestMode ->  InterestRate -> PaymentNo -> InterestRate
interestFactorForPaymentNo ThreeSixtyFiveMonth accrualRate terms = monthlyInterest ThreeSixtyFiveMonth accrualRate terms
interestFactorForPaymentNo ThreeSixtyMonth accrualRate terms = monthlyInterest ThreeSixtyMonth accrualRate terms
interestFactorForPaymentNo TwoFiveTwoMonth accrualRate terms = monthlyInterest TwoFiveTwoMonth accrualRate terms
interestFactorForPaymentNo ThreeSixtyFiveDay accrualRate terms = dailyInterest ThreeSixtyFiveDay accrualRate terms
interestFactorForPaymentNo ThreeSixtyDay accrualRate terms = dailyInterest ThreeSixtyDay accrualRate terms
interestFactorForPaymentNo TwoFiveTwoDay accrualRate terms = dailyInterest TwoFiveTwoDay accrualRate terms

compositeRate :: InterestRate -> Integer -> InterestRate
compositeRate r n = ((1 + r) ^ n) - 1

yearlyRate :: InterestMode -> InterestRate -> InterestRate
yearlyRate ThreeSixtyFiveMonth rate = compositeRate (rate / 30) 365
yearlyRate ThreeSixtyFiveDay rate   = compositeRate (rate / 30) 365
yearlyRate _ rate                   = compositeRate rate 12

-- Effective monthly rate adjusted for the year length (365 / 360)
effectiveRate :: InterestMode -> InterestRate -> InterestRate
effectiveRate mode rate = let
      yearlyRate' = yearlyRate mode rate
    in ((1 + yearlyRate') ** (1/12)) - 1

dailyRate :: InterestMode -> InterestRate -> InterestRate
dailyRate ThreeSixtyFiveDay rate = compositeRate (rate / 30) 1
dailyRate TwoFiveTwoDay rate = ((1 + yearlyRate ThreeSixtyFiveDay rate) ** (1/252)) - 1
dailyRate ThreeSixtyDay rate = ((1 + rate) ** (1 / 30)) - 1
dailyRate ThreeSixtyFiveMonth rate = ((1 + yearlyRate ThreeSixtyFiveDay rate) ** (1/365)) - 1
dailyRate ThreeSixtyMonth rate = ((1 + rate) ** (1 / 30)) - 1
dailyRate TwoFiveTwoMonth rate = ((1 + rate) ** (1 / 21)) - 1

monthlyInterest :: InterestMode -> InterestRate -> PaymentNo -> InterestRate
monthlyInterest mode accrualRate terms = let
  rate = effectiveRate mode accrualRate
  in ((1 + rate) ^ terms) -1

dailyInterest :: InterestMode -> InterestRate -> PaymentNo -> InterestRate
dailyInterest mode accrualRate terms = let
  rate = dailyRate mode accrualRate
  in ((1 + rate) ^ terms) - 1
