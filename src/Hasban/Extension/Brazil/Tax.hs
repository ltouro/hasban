{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Hasban.Extension.Brazil.Tax
Description : Brazilian IOF (Imposto sobre Operações Financeiras) tax calculations
Copyright   : (c) 2024
License     : MIT

This module provides Brazilian IOF tax calculations for loan operations.
IOF is a federal tax on financial operations in Brazil.

== IOF Tax Components

1. Base IOF: Fixed percentage on principal (default: 0.38%)
2. Daily IOF: Percentage per day, capped at 365 days (default: 0.0082%/day)

== Usage Example

@
import Hasban.Extension.Brazil.Tax

-- Calculate IOF for R$10,000 loan over 180 days
tax = calculateIOF defaultIOFRates (Money 10000) 180
-- Result: R$185.76 (0.38% + 0.0082% × 180 days)
@
-}
module Hasban.Extension.Brazil.Tax (
  -- * IOF Calculation
  calculateIOF
  , calculateIOFWithRates
  , iofForPayment

  -- * IOF Rate Configuration
  , IOFRates(..)
  , defaultIOFRates
  , zeroIOFRates

  -- * Helpers
  , dailyIOFCapped
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Hasban.Types (Money(..))

-- | IOF rate configuration
data IOFRates = IOFRates
  { iofBaseRate  :: Double  -- ^ Base IOF rate (e.g., 0.0038 for 0.38%)
  , iofDailyRate :: Double  -- ^ Daily additional rate (e.g., 0.000082 for 0.0082%)
  , iofMaxDays   :: Int     -- ^ Maximum days for daily rate (365)
  } deriving (Show, Eq, Generic)

instance FromJSON IOFRates
instance ToJSON IOFRates

-- | Default Brazilian IOF rates (as of 2024)
-- Base: 0.38% + Daily: 0.0082% per day, capped at 365 days
defaultIOFRates :: IOFRates
defaultIOFRates = IOFRates
  { iofBaseRate = 0.0038
  , iofDailyRate = 0.000082
  , iofMaxDays = 365
  }

-- | Zero IOF rates (for non-IOF scenarios)
zeroIOFRates :: IOFRates
zeroIOFRates = IOFRates
  { iofBaseRate = 0
  , iofDailyRate = 0
  , iofMaxDays = 365
  }

-- | Calculate total IOF tax using default rates
-- IOF = (base_rate × principal) + (daily_rate × min(days, 365) × principal)
calculateIOF :: Money -> Int -> Money
calculateIOF = calculateIOFWithRates defaultIOFRates

-- | Calculate IOF tax with custom rates
calculateIOFWithRates :: IOFRates -> Money -> Int -> Money
calculateIOFWithRates rates (Money principal) days =
  let cappedDays = min (iofMaxDays rates) days
      baseTax = iofBaseRate rates * principal
      dailyTax = iofDailyRate rates * fromIntegral cappedDays * principal
  in Money (baseTax + dailyTax)

-- | Calculate IOF for a specific amortization payment
-- Used when computing IOF on each principal payment in amortization schedule
iofForPayment :: IOFRates -> Money -> Int -> Money
iofForPayment rates (Money amortization) daysFromStart =
  let cappedDays = min (iofMaxDays rates) daysFromStart
      baseTax = iofBaseRate rates * amortization
      dailyTax = iofDailyRate rates * fromIntegral cappedDays * amortization
  in Money (baseTax + dailyTax)

-- | Calculate only the daily IOF component (capped)
dailyIOFCapped :: IOFRates -> Money -> Int -> Money
dailyIOFCapped rates (Money principal) days =
  let cappedDays = min (iofMaxDays rates) days
  in Money $ iofDailyRate rates * fromIntegral cappedDays * principal
