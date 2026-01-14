{- |
Module      : Hasban
Description : General-purpose loan calculation library
Copyright   : (c) 2024
License     : MIT

Hasban is a general-purpose loan calculation library for:
- Personal loans (amortizing): PRICE, SAC, and Bullet amortization
- Credit cards (revolving): Payoff projections and minimum payment analysis

== Quick Start

=== Personal Loan Example

@
import Hasban

-- $10,000 loan at 8% APR for 36 months
loan = PersonalLoan
  { plPrincipal    = Money 10000
  , plAnnualRate   = Rate 0.08
  , plTerms        = 36
  , plStartDate    = toDate (2024, 1, 15)
  , plAmortization = Price
  , plFrequency    = Monthly
  }

payment = monthlyPayment loan  -- Money 313.36
schedule = generateSchedule loan
@

=== Credit Card Example

@
import Hasban

-- Credit card with $10k limit, 22% APR, $100 or 2% minimum
card = CreditCard
  { ccLimit      = Money 10000
  , ccAnnualRate = Rate 0.22
  , ccMinPayment = GreaterOf (Money 100) (Rate 0.02)
  }

balance = Money 5000
stmt = analyzeBalance card balance
-- Shows: 94 months to payoff, $4367 total interest at minimum payment
@

-}
module Hasban (
  -- * Core Types
  Money(..)
  , Rate(..)
  , mkMoney
  , mkRate

  -- * Date Types
  , Date(..)
  , Frequency(..)
  , toDate

  -- * Personal Loans
  , PersonalLoan(..)
  , AmortizationType(..)
  , Payment(..)
  , InterestMethod(..)

  -- * Fees and Commissions
  , Fee(..)
  , FeeType(..)

  -- * Grace Periods
  , GracePeriod(..)

  -- ** Loan Calculations
  , monthlyPayment
  , periodicPayment
  , totalInterest
  , totalPayments
  , generateSchedule
  , generateScheduleWithGrace

  -- ** Fee Calculations
  , totalFees
  , feeAmount
  , netDisbursement

  -- ** Net Value Loan (Goal Seek)
  , findPrincipalForNetValue

  -- ** Rate Conversions
  , effectiveRate
  , periodicRate
  , annualToMonthly
  , interestFactor
  , compoundFactor

  -- * Credit Cards
  , CreditCard(..)
  , MinPayment(..)
  , CreditCardStatement(..)
  , PayoffMonth(..)
  , PayoffProjection(..)

  -- ** Credit Card Calculations
  , analyzeBalance
  , minimumPayment
  , payoffProjection
  , simulatePayoff
  , trueCostOfPurchase

  -- * Validation
  , ValidationError(..)
  , ValidationResult
  , validatePersonalLoan
  , validateCreditCard
  , validatePrincipal
  , validateRate
  , validateTerms

  -- * Display / Formatting
  , formatSchedule
  , formatScheduleMarkdown
  , formatScheduleCSV
) where

-- Re-export core types
import Hasban.Types

-- Re-export loan calculations
import Hasban.Loan (
    monthlyPayment
  , periodicPayment
  , totalInterest
  , totalPayments
  , effectiveRate
  , periodicRate
  , annualToMonthly
  , interestFactor
  , compoundFactor
  , totalFees
  , feeAmount
  , netDisbursement
  )

-- Re-export amortization
import Hasban.Amortization (
    generateSchedule
  , generateScheduleWithGrace
  )

-- Re-export credit card
import Hasban.CreditCard (
    analyzeBalance
  , minimumPayment
  , payoffProjection
  , simulatePayoff
  , trueCostOfPurchase
  )

-- Re-export validation
import Hasban.Validation (
    ValidationError(..)
  , ValidationResult
  , validatePersonalLoan
  , validateCreditCard
  , validatePrincipal
  , validateRate
  , validateTerms
  )

-- For formatting
import Date (formatDate)
import Text.Printf (printf)

-- ============================================
-- NET VALUE LOAN (GOAL SEEK)
-- ============================================

-- | Find principal amount that yields desired net disbursement after fees
-- Uses Newton-Raphson iteration to solve: principal - fees(principal) = desiredNet
findPrincipalForNetValue
  :: Money           -- ^ Desired net disbursement
  -> Rate            -- ^ Interest rate
  -> Int             -- ^ Terms
  -> [Fee]           -- ^ Fees to deduct
  -> Maybe Money     -- ^ Required principal (Nothing if no solution)
findPrincipalForNetValue (Money desiredNet) rate terms fees =
  -- Create a loan template to calculate fees
  -- We need to find P such that: P - totalFees(P) = desiredNet
  -- Rearranging: P - (fixed_fees + pct_fees * P) = desiredNet
  --              P * (1 - pct_fees) = desiredNet + fixed_fees
  --              P = (desiredNet + fixed_fees) / (1 - pct_fees)
  let (fixedTotal, pctTotal) = separateFees fees
      divisor = 1 - pctTotal
  in if divisor <= 0
     then Nothing  -- Percentage fees >= 100%, no solution
     else
       let principal = (desiredNet + fixedTotal) / divisor
       in if principal > 0
          then Just (Money principal)
          else Nothing
  where
    separateFees :: [Fee] -> (Double, Double)
    separateFees fs = foldr addFee (0, 0) fs

    addFee :: Fee -> (Double, Double) -> (Double, Double)
    addFee (Fee FixedFee (Money amt)) (fixed, pct) = (fixed + amt, pct)
    addFee (Fee PercentageFee (Money p)) (fixed, pct) = (fixed, pct + p)

-- ============================================
-- DISPLAY / FORMATTING
-- ============================================

-- | Format payment schedule as ASCII table
formatSchedule :: [Payment] -> String
formatSchedule payments =
  let header = "#     Date         Interest      Principal     Balance       Payment       Tax\n"
      separator = replicate 85 '-' ++ "\n"
      rows = map formatPaymentRow payments
  in header ++ separator ++ concat rows

formatPaymentRow :: Payment -> String
formatPaymentRow p =
  printf "%-5d %-12s %12.2f  %12.2f  %12.2f  %12.2f  %s\n"
    (paymentNumber p)
    (formatDate (paymentDate p))
    (unMoney (paymentInterest p))
    (unMoney (paymentPrincipal p))
    (unMoney (paymentBalance p))
    (unMoney (paymentTotal p))
    (maybe "       -" (printf "%8.2f" . unMoney) (paymentTax p))

-- | Format payment schedule as Markdown table
formatScheduleMarkdown :: [Payment] -> String
formatScheduleMarkdown payments =
  let header = "| # | Date | Interest | Principal | Balance | Payment | Tax |\n"
      separator = "|---|------|----------|-----------|---------|---------|-----|\n"
      rows = map formatPaymentMarkdown payments
  in header ++ separator ++ concat rows

formatPaymentMarkdown :: Payment -> String
formatPaymentMarkdown p =
  printf "| %d | %s | %.2f | %.2f | %.2f | %.2f | %s |\n"
    (paymentNumber p)
    (formatDate (paymentDate p))
    (unMoney (paymentInterest p))
    (unMoney (paymentPrincipal p))
    (unMoney (paymentBalance p))
    (unMoney (paymentTotal p))
    (maybe "-" (printf "%.2f" . unMoney) (paymentTax p))

-- | Format payment schedule as CSV
formatScheduleCSV :: [Payment] -> String
formatScheduleCSV payments =
  let header = "Number,Date,Interest,Principal,Balance,Payment,Tax\n"
      rows = map formatPaymentCSV payments
  in header ++ concat rows

formatPaymentCSV :: Payment -> String
formatPaymentCSV p =
  printf "%d,%s,%.2f,%.2f,%.2f,%.2f,%s\n"
    (paymentNumber p)
    (formatDate (paymentDate p))
    (unMoney (paymentInterest p))
    (unMoney (paymentPrincipal p))
    (unMoney (paymentBalance p))
    (unMoney (paymentTotal p))
    (maybe "" (printf "%.2f" . unMoney) (paymentTax p))
