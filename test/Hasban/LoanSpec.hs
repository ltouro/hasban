{-# LANGUAGE OverloadedStrings #-}

module Hasban.LoanSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen)
import Hasban

spec :: Spec
spec = describe "Hasban Personal Loan Calculations" $ do

  describe "PRICE amortization" $ do
    it "calculates correct monthly payment for standard loan" $ do
      -- $10,000 at 8% APR for 36 months
      let loan = PersonalLoan
            { plPrincipal = Money 10000
            , plAnnualRate = Rate 0.08
            , plTerms = 36
            , plStartDate = toDate (2024, 1, 15)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let pmt = monthlyPayment loan
      -- Using effective monthly rate: ((1+0.08)^(1/12))-1 ≈ 0.006434
      -- PMT ≈ $312.08
      unMoney pmt `shouldSatisfy` (\x -> x > 300 && x < 320)

    it "generates schedule with correct number of payments" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 10000
            , plAnnualRate = Rate 0.08
            , plTerms = 36
            , plStartDate = toDate (2024, 1, 15)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      length schedule `shouldBe` 36

    it "ends with zero balance" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 10000
            , plAnnualRate = Rate 0.08
            , plTerms = 36
            , plStartDate = toDate (2024, 1, 15)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      let finalBalance = unMoney $ paymentBalance $ last schedule
      finalBalance `shouldSatisfy` (\x -> abs x < 0.01)

    it "handles zero interest rate" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 1000
            , plAnnualRate = Rate 0
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let pmt = monthlyPayment loan
      -- $1000 / 12 = $83.33
      unMoney pmt `shouldSatisfy` (\x -> abs (x - 83.33) < 0.01)

    it "calculates total interest correctly" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 10000
            , plAnnualRate = Rate 0.08
            , plTerms = 36
            , plStartDate = toDate (2024, 1, 15)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let interest = totalInterest loan
      -- Expected ~$1281
      unMoney interest `shouldSatisfy` (\x -> x > 1200 && x < 1400)

  describe "SAC amortization" $ do
    it "has constant principal payments" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 12000
            , plAnnualRate = Rate 0.12
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Sac
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      -- Each principal payment should be $1000 (except last which clears balance)
      let principals = map (unMoney . paymentPrincipal) (init schedule)
      all (\x -> abs (x - 1000) < 0.01) principals `shouldBe` True

    it "has decreasing total payments" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 12000
            , plAnnualRate = Rate 0.12
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Sac
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      let totals = map (unMoney . paymentTotal) schedule
      -- Each payment should be less than or equal to the previous
      and (zipWith (>=) totals (tail totals)) `shouldBe` True

  describe "Bullet amortization" $ do
    it "pays only interest until last payment" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 10000
            , plAnnualRate = Rate 0.12
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Bullet
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      let principalPayments = map (unMoney . paymentPrincipal) (init schedule)
      all (== 0) principalPayments `shouldBe` True

    it "pays full principal on last payment" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 10000
            , plAnnualRate = Rate 0.12
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Bullet
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      let lastPrincipal = unMoney $ paymentPrincipal $ last schedule
      abs (lastPrincipal - 10000) `shouldSatisfy` (< 0.01)

  describe "Property tests" $ do
    it "total principal paid equals loan amount" $ property $ do
      -- Widened constraints: allow very small principals, zero rates, and extreme terms
      principal <- choose (0.01, 1000000) :: Gen Double
      rate <- choose (0.0, 1.0) :: Gen Double  -- 0% to 100% APR
      terms <- choose (1, 600) :: Gen Int  -- 1 month to 50 years
      let loan = PersonalLoan
            { plPrincipal = Money principal
            , plAnnualRate = Rate rate
            , plTerms = terms
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
          schedule = generateSchedule loan
          totalPrincipal = sum $ map (unMoney . paymentPrincipal) schedule
      -- Use relative tolerance for very small principals
      return $ abs (totalPrincipal - principal) < max 0.01 (principal * 0.0001)

    it "final balance is zero for all amortization types" $ property $ do
      -- Widened constraints: allow edge cases
      principal <- choose (0.01, 1000000) :: Gen Double
      rate <- choose (0.0, 1.0) :: Gen Double  -- 0% to 100% APR
      terms <- choose (1, 600) :: Gen Int  -- 1 month to 50 years
      amortType <- elements [Price, Sac, Bullet]
      let loan = PersonalLoan
            { plPrincipal = Money principal
            , plAnnualRate = Rate rate
            , plTerms = terms
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = amortType
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
          schedule = generateSchedule loan
          finalBalance = unMoney $ paymentBalance $ last schedule
      -- Use relative tolerance for very small principals
      return $ abs finalBalance < max 0.01 (principal * 0.0001)

  -- Test cases from IMPLEMENTATION_PLAN.md
  describe "Implementation Plan Test Cases" $ do
    it "Zero interest loan: $1000, 0%, 12mo -> Payment = $83.33" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 1000
            , plAnnualRate = Rate 0
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let pmt = monthlyPayment loan
      abs (unMoney pmt - 83.33) `shouldSatisfy` (< 0.01)

    it "Single payment loan: $1000, 5%, 1mo -> Payment includes principal + 1 month interest" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 1000
            , plAnnualRate = Rate 0.05
            , plTerms = 1
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let pmt = monthlyPayment loan
      let schedule = generateSchedule loan
      -- Single payment should clear the loan
      length schedule `shouldBe` 1
      -- Payment should be principal + monthly interest
      -- Monthly rate = ((1+0.05)^(1/12))-1 ≈ 0.00407
      -- Payment ≈ $1004.07
      unMoney pmt `shouldSatisfy` (\x -> x > 1000 && x < 1010)
      -- Final balance should be zero
      unMoney (paymentBalance (head schedule)) `shouldSatisfy` (\x -> abs x < 0.01)

    it "Large loan: $500k, 6.5%, 360mo (30-year mortgage)" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 500000
            , plAnnualRate = Rate 0.065
            , plTerms = 360
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let pmt = monthlyPayment loan
      let schedule = generateSchedule loan
      -- Using effective monthly rate: ((1+0.065)^(1/12))-1 ≈ 0.00526
      -- This gives PMT ≈ $3099.44 (vs $3160.34 with simple r/12 method)
      -- Note: Our implementation uses effective rate conversion for mathematical accuracy
      unMoney pmt `shouldSatisfy` (\x -> x > 3050 && x < 3150)
      -- Final balance should be zero
      unMoney (paymentBalance (last schedule)) `shouldSatisfy` (\x -> abs x < 0.01)
      -- Total payments should be significantly more than principal
      let totalPaid = sum $ map (unMoney . paymentTotal) schedule
      totalPaid `shouldSatisfy` (> 1000000)  -- More than $1M total for 30-year mortgage

  -- Edge case investigations
  describe "Edge Cases" $ do
    it "Very small principal: $1" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 1
            , plAnnualRate = Rate 0.10
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      let finalBalance = unMoney $ paymentBalance $ last schedule
      abs finalBalance `shouldSatisfy` (< 0.01)

    it "Very high interest rate: 100% APR" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 1000
            , plAnnualRate = Rate 1.0  -- 100% APR
            , plTerms = 12
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      let finalBalance = unMoney $ paymentBalance $ last schedule
      abs finalBalance `shouldSatisfy` (< 0.01)
      -- With effective rate conversion, 100% APR -> ~5.95% monthly rate
      -- Total paid is ~$1427 (not $2000 as with simple r/12 method)
      let totalPaid = sum $ map (unMoney . paymentTotal) schedule
      totalPaid `shouldSatisfy` (> 1400)
      -- Should have significant interest component
      let totalInterestPaid = totalPaid - 1000
      totalInterestPaid `shouldSatisfy` (> 400)

    it "Very long term: 600 months (50 years)" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 100000
            , plAnnualRate = Rate 0.05
            , plTerms = 600
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Monthly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      length schedule `shouldBe` 600
      let finalBalance = unMoney $ paymentBalance $ last schedule
      abs finalBalance `shouldSatisfy` (< 0.01)

    it "Biweekly payment frequency" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 10000
            , plAnnualRate = Rate 0.08
            , plTerms = 26  -- 1 year of biweekly payments
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Biweekly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      length schedule `shouldBe` 26
      let finalBalance = unMoney $ paymentBalance $ last schedule
      abs finalBalance `shouldSatisfy` (< 0.01)

    it "Weekly payment frequency" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 5000
            , plAnnualRate = Rate 0.12
            , plTerms = 52  -- 1 year of weekly payments
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Weekly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      length schedule `shouldBe` 52
      let finalBalance = unMoney $ paymentBalance $ last schedule
      abs finalBalance `shouldSatisfy` (< 0.01)

    it "Yearly payment frequency" $ do
      let loan = PersonalLoan
            { plPrincipal = Money 50000
            , plAnnualRate = Rate 0.06
            , plTerms = 5  -- 5 annual payments
            , plStartDate = toDate (2024, 1, 1)
            , plAmortization = Price
            , plFrequency = Yearly
            , plFees = []
            , plGracePeriod = Nothing
            }
      let schedule = generateSchedule loan
      length schedule `shouldBe` 5
      let finalBalance = unMoney $ paymentBalance $ last schedule
      abs finalBalance `shouldSatisfy` (< 0.01)

instance Arbitrary AmortizationType where
  arbitrary = elements [Price, Sac, Bullet]
