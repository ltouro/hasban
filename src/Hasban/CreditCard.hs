{-# LANGUAGE DeriveGeneric #-}

module Hasban.CreditCard (
  -- * Statement Analysis
  analyzeBalance
  , minimumPayment

  -- * Payoff Projections
  , payoffProjection
  , simulatePayoff
  , monthsToPayoff

  -- * Cost Analysis
  , trueCostOfPurchase
  , interestAtMinimum
) where

import Hasban.Types

-- | Analyze a credit card balance and generate statement details
analyzeBalance :: CreditCard -> Money -> CreditCardStatement
analyzeBalance card balance =
  let minPmt = minimumPayment card balance
      monthlyRate = monthlyInterestRate card
      (months, totalInterest') = simulatePayoffSummary card balance minPmt
  in CreditCardStatement
    { stmtBalance = balance
    , stmtMinPayment = minPmt
    , stmtInterestRate = monthlyRate
    , stmtPayoffMonths = if unMoney balance <= 0 then Just 0 else months
    , stmtTotalInterest = totalInterest'
    }

-- | Calculate minimum payment based on card rules
minimumPayment :: CreditCard -> Money -> Money
minimumPayment card (Money balance)
  | balance <= 0 = Money 0
  | otherwise = case ccMinPayment card of
      FixedAmount fixed -> min (Money balance) fixed
      PercentOfBalance (Rate pct) -> Money $ max 0 (balance * pct)
      GreaterOf (Money fixed) (Rate pct) ->
        let percentAmount = balance * pct
            minAmount = max fixed percentAmount
        in Money $ min balance minAmount

-- | Get monthly interest rate from annual rate
monthlyInterestRate :: CreditCard -> Rate
monthlyInterestRate card =
  let annualRate = unRate (ccAnnualRate card)
  in Rate (annualRate / 12)

-- | Project payoff at a given payment amount
payoffProjection :: CreditCard -> Money -> Money -> PayoffProjection
payoffProjection card balance payment =
  let schedule = simulatePayoff card balance payment
      totalPaid = sum [unMoney (pmPayment p) | p <- schedule]
      totalInterest' = sum [unMoney (pmInterest p) | p <- schedule]
  in PayoffProjection
    { ppMonths = length schedule
    , ppTotalPaid = Money totalPaid
    , ppTotalInterest = Money totalInterest'
    }

-- | Simulate month-by-month payoff
simulatePayoff :: CreditCard -> Money -> Money -> [PayoffMonth]
simulatePayoff card initialBalance fixedPayment =
  go 1 (unMoney initialBalance)
  where
    monthlyRate = unRate (monthlyInterestRate card)
    maxMonths = 1200  -- 100 years cap to prevent infinite loops

    go :: Int -> Double -> [PayoffMonth]
    go month balance
      | balance <= 0 = []
      | month > maxMonths = []  -- Safety cap
      | otherwise =
          let interest = balance * monthlyRate
              -- Payment is minimum of fixed payment or remaining balance + interest
              actualPayment = min (unMoney fixedPayment) (balance + interest)
              principalPaid = actualPayment - interest
              newBalance = max 0 (balance - principalPaid)
              payoffMonth = PayoffMonth
                { pmMonth = month
                , pmPayment = Money actualPayment
                , pmInterest = Money interest
                , pmPrincipal = Money principalPaid
                , pmBalance = Money newBalance
                }
          in if principalPaid <= 0 && balance > 0
             then []  -- Payment doesn't cover interest, would never pay off
             else payoffMonth : go (month + 1) newBalance

-- | Calculate months to payoff (Nothing if payment too low)
monthsToPayoff :: CreditCard -> Money -> Money -> Maybe Int
monthsToPayoff card balance payment =
  let schedule = simulatePayoff card balance payment
  in if null schedule && unMoney balance > 0
     then Nothing  -- Can't pay off
     else Just (length schedule)

-- | Internal: simulate and return (months, total interest)
simulatePayoffSummary :: CreditCard -> Money -> Money -> (Maybe Int, Money)
simulatePayoffSummary card balance payment =
  let schedule = simulatePayoff card balance payment
  in if null schedule && unMoney balance > 0
     then (Nothing, Money 0)
     else (Just (length schedule), Money $ sum [unMoney (pmInterest p) | p <- schedule])

-- ============================================
-- COST ANALYSIS
-- ============================================

-- | Calculate true cost of a purchase if paid at minimum payment
-- Returns (months to pay, total interest cost)
trueCostOfPurchase :: CreditCard -> Money -> (Int, Money)
trueCostOfPurchase card purchaseAmount =
  let minPmt = minimumPayment card purchaseAmount
      schedule = simulatePayoff card purchaseAmount minPmt
      totalInterest' = sum [unMoney (pmInterest p) | p <- schedule]
  in (length schedule, Money totalInterest')

-- | Calculate total interest if only paying minimum
interestAtMinimum :: CreditCard -> Money -> Money
interestAtMinimum card balance =
  let minPmt = minimumPayment card balance
      (_, totalInterest') = simulatePayoffSummary card balance minPmt
  in totalInterest'
