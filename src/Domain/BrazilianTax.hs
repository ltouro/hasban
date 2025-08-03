{-# LANGUAGE DeriveGeneric #-}

module Domain.BrazilianTax
  ( IOFTax(..)
  , IOFRates(..)
  , calculateIOF
  , standardIOFRates
  , calculateDailyIOF
  , calculateFixedIOF
  , totalIOFForAmortization
  ) where

import Domain.Types (TaxAmount(..), CurrencyAmount, PersonType(..), unCurrencyAmount)
import Infrastructure.DateCalculator (FinancialDate, daysBetween)

-- | IOF (Imposto sobre Operações Financeiras) tax configuration
-- Based on Brazilian law and B3 methodology
data IOFRates = IOFRates
  { iofFixedRate :: Double    -- ^ Fixed IOF rate (typically 0.0038 = 0.38%)
  , iofDailyRate :: Double    -- ^ Daily IOF rate (varies by person type)
  , maxDays      :: Integer   -- ^ Maximum days for daily IOF calculation (typically 365)
  } deriving (Eq, Show)

-- | IOF tax calculation result
data IOFTax = IOFTax
  { fixedIOF :: TaxAmount    -- ^ Fixed portion of IOF
  , dailyIOF :: TaxAmount    -- ^ Daily portion of IOF  
  , totalIOF :: TaxAmount    -- ^ Total IOF amount
  , daysCounted :: Integer   -- ^ Number of days used in calculation
  } deriving (Eq, Show)

-- | Standard IOF rates according to Brazilian regulation
-- Law 13.043/2014 and subsequent regulations
standardIOFRates :: PersonType -> IOFRates
standardIOFRates personType =
  IOFRates
    { iofFixedRate = 0.0038  -- 0.38% fixed rate
    , iofDailyRate = case personType of
        Natural -> 0.000082  -- 0.0082% per day for individuals
        Legal   -> 0.000041  -- 0.0041% per day for companies
    , maxDays = 365          -- Maximum 365 days for daily calculation
    }

-- | Calculate IOF tax for a loan amortization
-- 
-- Brazilian IOF has two components:
-- 1. Fixed IOF: Applied once on the principal amount
-- 2. Daily IOF: Applied daily from loan start to payment date (max 365 days)
--
-- Reference: B3 "Caderno de Fórmulas" methodology
calculateIOF :: IOFRates 
             -> CurrencyAmount      -- ^ Principal amount
             -> FinancialDate       -- ^ Loan start date
             -> FinancialDate       -- ^ Payment due date
             -> IOFTax
calculateIOF rates principal startDate dueDate =
  let principalValue = unCurrencyAmount principal
      daysElapsed = daysBetween startDate dueDate
      cappedDays = min (maxDays rates) daysElapsed
      
      fixedAmount = calculateFixedIOF rates principal
      dailyAmount = calculateDailyIOF rates principal cappedDays
      totalAmount = TaxAmount (unTaxAmount fixedAmount + unTaxAmount dailyAmount)
      
  in IOFTax
      { fixedIOF = fixedAmount
      , dailyIOF = dailyAmount  
      , totalIOF = totalAmount
      , daysCounted = cappedDays
      }

-- | Calculate the fixed portion of IOF
calculateFixedIOF :: IOFRates -> CurrencyAmount -> TaxAmount
calculateFixedIOF rates principal =
  TaxAmount (iofFixedRate rates * unCurrencyAmount principal)

-- | Calculate the daily portion of IOF
calculateDailyIOF :: IOFRates -> CurrencyAmount -> Integer -> TaxAmount
calculateDailyIOF rates principal days =
  TaxAmount (iofDailyRate rates * fromIntegral days * unCurrencyAmount principal)

-- | Calculate total IOF for an amortization payment
-- This is a convenience function for single payment calculations
totalIOFForAmortization :: PersonType 
                        -> CurrencyAmount 
                        -> FinancialDate 
                        -> FinancialDate 
                        -> TaxAmount
totalIOFForAmortization personType principal startDate dueDate =
  let rates = standardIOFRates personType
      iofResult = calculateIOF rates principal startDate dueDate
  in totalIOF iofResult

{- |
IOF Calculation Notes:

The IOF (Imposto sobre Operações Financeiras) is a Brazilian federal tax that applies
to financial operations, including loans. The calculation follows specific rules:

1. Fixed IOF (0.38%): Applied once to the principal amount at the beginning of the loan.

2. Daily IOF: Applied daily from the loan origination date to each payment date.
   - For individuals (Natural): 0.0082% per day
   - For companies (Legal): 0.0041% per day
   - Maximum of 365 days counted

3. The total IOF for each payment is: Fixed IOF + (Daily IOF × days × principal)

4. For amortizing loans, the IOF is calculated on the amortization portion of each payment,
   not on the interest portion.

References:
- Law 13.043/2014
- B3 "Caderno de Fórmulas - CCB, CCE e NCE"
- Brazilian Central Bank regulations
-}