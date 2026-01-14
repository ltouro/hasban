{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hasban.Amortization (
  -- * Schedule Generation
  generateSchedule
  , generateScheduleWithGrace
  , generatePriceSchedule
  , generateSacSchedule
  , generateBulletSchedule

  -- * Single Payment Calculations
  , calculatePayment
  , calculatePricePayment
  , calculateSacPayment
  , calculateBulletPayment

  -- * Summary Functions
  , totalInterestPaid
  , totalPrincipalPaid
  , scheduleBalance
  , effectiveAnnualRate
) where

import Hasban.Types
import Hasban.Loan (periodicRate)
import Date (addTerm)

-- | Generate full payment schedule for any amortization type
-- Handles grace periods if present
generateSchedule :: PersonalLoan -> [Payment]
generateSchedule loan =
  case plGracePeriod loan of
    Nothing -> generateBaseSchedule loan
    Just gp -> generateScheduleWithGrace loan gp

-- | Generate schedule without grace period
generateBaseSchedule :: PersonalLoan -> [Payment]
generateBaseSchedule loan =
  case plAmortization loan of
    Price  -> generatePriceSchedule loan
    Sac    -> generateSacSchedule loan
    Bullet -> generateBulletSchedule loan

-- | Generate schedule with grace period (interest-only payments initially)
generateScheduleWithGrace :: PersonalLoan -> GracePeriod -> [Payment]
generateScheduleWithGrace loan gp =
  let rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      principal = unMoney (plPrincipal loan)
      graceTerms = gpTerms gp
      startDate = plStartDate loan
      freq = plFrequency loan

      -- Generate grace period payments (interest only)
      gracePayments = [makeGracePayment i rate principal startDate freq | i <- [1..graceTerms]]

      -- Calculate effective principal after grace period
      effectivePrincipal = if gpCapitalize gp
        then principal * (1 + rate) ^ graceTerms  -- Capitalize interest
        else principal  -- Keep original principal

      -- Create modified loan for remaining amortization
      remainingLoan = loan
        { plPrincipal = Money effectivePrincipal
        , plTerms = plTerms loan - graceTerms
        , plStartDate = addTerm startDate freq graceTerms
        , plGracePeriod = Nothing  -- Clear grace period for recursion
        }

      -- Generate remaining schedule
      remainingSchedule = generateBaseSchedule remainingLoan

      -- Renumber remaining payments
      renumberedSchedule = [p { paymentNumber = paymentNumber p + graceTerms } | p <- remainingSchedule]

  in gracePayments ++ renumberedSchedule
  where
    makeGracePayment :: Int -> Double -> Double -> Date -> Frequency -> Payment
    makeGracePayment payNum rate' balance startDate' freq' =
      let interest = balance * rate'
          payDate = addTerm startDate' freq' payNum
      in Payment
        { paymentNumber = payNum
        , paymentDate = payDate
        , paymentInterest = Money interest
        , paymentPrincipal = Money 0  -- No principal during grace
        , paymentTotal = Money interest
        , paymentBalance = Money balance
        , paymentTax = Nothing
        }

-- | Generate PRICE (constant payment) schedule
generatePriceSchedule :: PersonalLoan -> [Payment]
generatePriceSchedule loan = go 1 (unMoney (plPrincipal loan))
  where
    rate = periodicRate (plAnnualRate loan) (plFrequency loan)
    n = plTerms loan
    startDate = plStartDate loan
    freq = plFrequency loan
    pmt = let principal = unMoney (plPrincipal loan)
          in if rate == 0
             then principal / fromIntegral n
             else let factor = (1 + rate) ^ n
                  in principal * (rate * factor) / (factor - 1)

    go :: Int -> Double -> [Payment]
    go payNum balance
      | payNum > n = []
      | otherwise =
          let interest = balance * rate
              -- Last payment adjusts for rounding
              actualPmt = if payNum == n
                          then balance + interest
                          else pmt
              principalPaid = actualPmt - interest
              newBalance = max 0 (balance - principalPaid)
              payDate = addTerm startDate freq payNum
              payment = Payment
                { paymentNumber = payNum
                , paymentDate = payDate
                , paymentInterest = Money interest
                , paymentPrincipal = Money principalPaid
                , paymentTotal = Money actualPmt
                , paymentBalance = Money newBalance
                , paymentTax = Nothing
                }
          in payment : go (payNum + 1) newBalance

-- | Generate SAC (constant amortization) schedule
generateSacSchedule :: PersonalLoan -> [Payment]
generateSacSchedule loan = go 1 principal
  where
    principal = unMoney (plPrincipal loan)
    rate = periodicRate (plAnnualRate loan) (plFrequency loan)
    n = plTerms loan
    startDate = plStartDate loan
    freq = plFrequency loan
    amortization = principal / fromIntegral n

    go :: Int -> Double -> [Payment]
    go payNum balance
      | payNum > n = []
      | otherwise =
          let interest = balance * rate
              -- Last payment clears remaining balance
              principalPaid = if payNum == n
                              then balance
                              else amortization
              totalPmt = interest + principalPaid
              newBalance = max 0 (balance - principalPaid)
              payDate = addTerm startDate freq payNum
              payment = Payment
                { paymentNumber = payNum
                , paymentDate = payDate
                , paymentInterest = Money interest
                , paymentPrincipal = Money principalPaid
                , paymentTotal = Money totalPmt
                , paymentBalance = Money newBalance
                , paymentTax = Nothing
                }
          in payment : go (payNum + 1) newBalance

-- | Generate Bullet (interest-only) schedule
generateBulletSchedule :: PersonalLoan -> [Payment]
generateBulletSchedule loan = go 1 principal
  where
    principal = unMoney (plPrincipal loan)
    rate = periodicRate (plAnnualRate loan) (plFrequency loan)
    n = plTerms loan
    startDate = plStartDate loan
    freq = plFrequency loan

    go :: Int -> Double -> [Payment]
    go payNum balance
      | payNum > n = []
      | otherwise =
          let interest = balance * rate
              -- Principal only paid on last payment
              principalPaid = if payNum == n then balance else 0
              totalPmt = interest + principalPaid
              newBalance = if payNum == n then 0 else balance
              payDate = addTerm startDate freq payNum
              payment = Payment
                { paymentNumber = payNum
                , paymentDate = payDate
                , paymentInterest = Money interest
                , paymentPrincipal = Money principalPaid
                , paymentTotal = Money totalPmt
                , paymentBalance = Money newBalance
                , paymentTax = Nothing
                }
          in payment : go (payNum + 1) newBalance

-- ============================================
-- SINGLE PAYMENT CALCULATIONS
-- ============================================

-- | Calculate payment details for a specific payment number
calculatePayment :: PersonalLoan -> Int -> Money -> Payment
calculatePayment loan payNum currentBalance =
  case plAmortization loan of
    Price  -> calculatePricePayment loan payNum currentBalance
    Sac    -> calculateSacPayment loan payNum currentBalance
    Bullet -> calculateBulletPayment loan payNum currentBalance

-- | Calculate single PRICE payment
calculatePricePayment :: PersonalLoan -> Int -> Money -> Payment
calculatePricePayment loan payNum (Money balance) =
  let rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      n = plTerms loan
      principal = unMoney (plPrincipal loan)
      pmt = if rate == 0
            then principal / fromIntegral n
            else let factor = (1 + rate) ^ n
                 in principal * (rate * factor) / (factor - 1)
      interest = balance * rate
      actualPmt = if payNum == n then balance + interest else pmt
      principalPaid = actualPmt - interest
      newBalance = max 0 (balance - principalPaid)
      payDate = addTerm (plStartDate loan) (plFrequency loan) payNum
  in Payment payNum payDate (Money interest) (Money principalPaid) (Money actualPmt) (Money newBalance) Nothing

-- | Calculate single SAC payment
calculateSacPayment :: PersonalLoan -> Int -> Money -> Payment
calculateSacPayment loan payNum (Money balance) =
  let rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      n = plTerms loan
      principal = unMoney (plPrincipal loan)
      amortization = principal / fromIntegral n
      interest = balance * rate
      principalPaid = if payNum == n then balance else amortization
      totalPmt = interest + principalPaid
      newBalance = max 0 (balance - principalPaid)
      payDate = addTerm (plStartDate loan) (plFrequency loan) payNum
  in Payment payNum payDate (Money interest) (Money principalPaid) (Money totalPmt) (Money newBalance) Nothing

-- | Calculate single bullet payment
calculateBulletPayment :: PersonalLoan -> Int -> Money -> Payment
calculateBulletPayment loan payNum (Money balance) =
  let rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      n = plTerms loan
      interest = balance * rate
      principalPaid = if payNum == n then balance else 0
      totalPmt = interest + principalPaid
      newBalance = if payNum == n then 0 else balance
      payDate = addTerm (plStartDate loan) (plFrequency loan) payNum
  in Payment payNum payDate (Money interest) (Money principalPaid) (Money totalPmt) (Money newBalance) Nothing

-- ============================================
-- SUMMARY FUNCTIONS
-- ============================================

-- | Calculate total interest from payment schedule
totalInterestPaid :: [Payment] -> Money
totalInterestPaid payments =
  Money $ sum [unMoney (paymentInterest p) | p <- payments]

-- | Calculate total principal from payment schedule
totalPrincipalPaid :: [Payment] -> Money
totalPrincipalPaid payments =
  Money $ sum [unMoney (paymentPrincipal p) | p <- payments]

-- | Get final balance from schedule (should be 0)
scheduleBalance :: [Payment] -> Money
scheduleBalance [] = Money 0
scheduleBalance payments = paymentBalance (last payments)

-- | Calculate effective annual rate from loan
effectiveAnnualRate :: PersonalLoan -> Rate
effectiveAnnualRate loan =
  let rate = periodicRate (plAnnualRate loan) (plFrequency loan)
      periods = frequencyPeriods (plFrequency loan)
  in Rate $ (1 + rate) ** periods - 1
  where
    frequencyPeriods Monthly   = 12
    frequencyPeriods Biweekly  = 26
    frequencyPeriods Weekly    = 52
    frequencyPeriods Daily     = 365
    frequencyPeriods Yearly    = 1
    frequencyPeriods EveryXDays = 365
    frequencyPeriods (CustomDays n) = 365 / fromIntegral n
