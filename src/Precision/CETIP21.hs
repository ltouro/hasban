{-# LANGUAGE DeriveGeneric #-}

module Precision.CETIP21 (
  PrecisionType(..),
  RoundingMode(..),
  applyPrecision,
  roundToDecimalPlaces,
  truncateToDecimalPlaces,
  validateMaxDecimalPlaces
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- CETIP21 Precision Requirements (MANDATORY COMPLIANCE)
-- - Interest factors: 9 decimal places WITH banker's rounding
-- - Unit values: 8 decimal places WITHOUT rounding (truncation)
-- - Financial values: 2 decimal places WITHOUT rounding (truncation)
-- - Pro-rata ratios: 9 decimal places WITHOUT rounding (truncation)
-- - Input rates: 4 decimal places maximum

data PrecisionType =
    InterestFactorPrecision  -- 9 decimal places WITH banker's rounding
  | UnitValuePrecision       -- 8 decimal places WITHOUT rounding (truncation)
  | FinancialValuePrecision  -- 2 decimal places WITHOUT rounding (truncation)
  | ProRataPrecision         -- 9 decimal places WITHOUT rounding (truncation)
  | InputRatePrecision       -- 4 decimal places maximum for input rates
  deriving (Eq, Show, Generic)

instance FromJSON PrecisionType
instance ToJSON PrecisionType

data RoundingMode =
    BankersRounding     -- Round to nearest even (0.5 -> 0, 1.5 -> 2)
  | TruncateOnly        -- Cut at decimal place, no rounding
  deriving (Eq, Show, Generic)

instance FromJSON RoundingMode
instance ToJSON RoundingMode

-- Apply CETIP21 precision rules (MANDATORY COMPLIANCE)
applyPrecision :: PrecisionType -> Double -> Double
applyPrecision InterestFactorPrecision r = roundToDecimalPlaces 9 BankersRounding r
applyPrecision UnitValuePrecision r = truncateToDecimalPlaces 8 r
applyPrecision FinancialValuePrecision r = truncateToDecimalPlaces 2 r
applyPrecision ProRataPrecision r = truncateToDecimalPlaces 9 r
applyPrecision InputRatePrecision r = validateMaxDecimalPlaces 4 r

-- CETIP21-specific banker's rounding implementation
roundToDecimalPlaces :: Int -> RoundingMode -> Double -> Double
roundToDecimalPlaces places BankersRounding value =
  let factor = 10.0 ^ places
      scaled = value * factor
      intPart = floor scaled :: Integer
      fractional = scaled - fromIntegral intPart
      -- Check if fractional part is exactly 0.5 (within floating point tolerance)
  in if abs (fractional - 0.5) < 1e-12
     then -- Banker's rounding: round to nearest even
       if even intPart
          then fromIntegral intPart / factor
          else fromIntegral (intPart + 1) / factor
     else fromIntegral (round scaled :: Integer) / factor
roundToDecimalPlaces places TruncateOnly value = truncateToDecimalPlaces places value

-- Truncation without rounding (CETIP21 requirement)
truncateToDecimalPlaces :: Int -> Double -> Double
truncateToDecimalPlaces places value =
  let factor = 10.0 ^ places
      scaled = value * factor
  in fromIntegral (truncate scaled :: Integer) / factor

-- Validate maximum decimal places for input rates
validateMaxDecimalPlaces :: Int -> Double -> Double
validateMaxDecimalPlaces maxPlaces value =
  let factor = 10.0 ^ maxPlaces
      scaled = value * factor
  in if scaled == fromIntegral (round scaled :: Integer)
     then value  -- Already within limit
     else truncateToDecimalPlaces maxPlaces value
