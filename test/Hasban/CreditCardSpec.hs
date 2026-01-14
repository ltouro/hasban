{-# LANGUAGE OverloadedStrings #-}

module Hasban.CreditCardSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Hasban

spec :: Spec
spec = describe "Hasban Credit Card Calculations" $ do

  describe "Minimum payment" $ do
    it "calculates fixed amount minimum correctly" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = FixedAmount (Money 25)
            }
      let minPmt = minimumPayment card (Money 1000)
      unMoney minPmt `shouldBe` 25

    it "calculates percentage minimum correctly" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = PercentOfBalance (Rate 0.02)
            }
      let minPmt = minimumPayment card (Money 1000)
      unMoney minPmt `shouldBe` 20  -- 2% of 1000

    it "calculates greater-of minimum correctly" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = GreaterOf (Money 25) (Rate 0.02)
            }
      -- For $1000 balance: max($25, $20) = $25
      let minPmt1 = minimumPayment card (Money 1000)
      unMoney minPmt1 `shouldBe` 25

      -- For $2000 balance: max($25, $40) = $40
      let minPmt2 = minimumPayment card (Money 2000)
      unMoney minPmt2 `shouldBe` 40

    it "caps minimum at balance" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = FixedAmount (Money 100)
            }
      let minPmt = minimumPayment card (Money 50)
      unMoney minPmt `shouldBe` 50

  describe "Balance analysis" $ do
    it "analyzes balance correctly" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = GreaterOf (Money 100) (Rate 0.02)
            }
      let stmt = analyzeBalance card (Money 5000)

      unMoney (stmtBalance stmt) `shouldBe` 5000
      unMoney (stmtMinPayment stmt) `shouldBe` 100  -- 2% of 5000 = 100
      -- Monthly rate = 22% / 12 ≈ 1.833%
      unRate (stmtInterestRate stmt) `shouldSatisfy` (\x -> abs (x - 0.01833) < 0.001)

    it "shows payoff takes many months at minimum" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = GreaterOf (Money 100) (Rate 0.02)
            }
      let stmt = analyzeBalance card (Money 5000)

      case stmtPayoffMonths stmt of
        Just months -> months `shouldSatisfy` (> 50)  -- Takes many years
        Nothing -> expectationFailure "Should have payoff months"

  describe "Payoff projection" $ do
    it "calculates payoff correctly" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.18
            , ccMinPayment = FixedAmount (Money 50)
            }
      let proj = payoffProjection card (Money 1000) (Money 50)

      ppMonths proj `shouldSatisfy` (> 20)  -- Takes more than 20 months
      unMoney (ppTotalPaid proj) `shouldSatisfy` (> 1000)  -- Pay more than principal

    it "higher payment means fewer months" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.18
            , ccMinPayment = FixedAmount (Money 50)
            }
      let proj50 = payoffProjection card (Money 1000) (Money 50)
      let proj100 = payoffProjection card (Money 1000) (Money 100)

      ppMonths proj100 `shouldSatisfy` (< ppMonths proj50)

    it "higher payment means less total interest" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.18
            , ccMinPayment = FixedAmount (Money 50)
            }
      let proj50 = payoffProjection card (Money 1000) (Money 50)
      let proj100 = payoffProjection card (Money 1000) (Money 100)

      unMoney (ppTotalInterest proj100) `shouldSatisfy`
        (< unMoney (ppTotalInterest proj50))

  describe "True cost of purchase" $ do
    it "shows purchases cost more than price" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = GreaterOf (Money 25) (Rate 0.02)
            }
      let (months, interest) = trueCostOfPurchase card (Money 500)

      months `shouldSatisfy` (> 10)
      unMoney interest `shouldSatisfy` (> 0)

  describe "Simulate payoff" $ do
    it "generates correct number of months" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.12
            , ccMinPayment = FixedAmount (Money 100)
            }
      let schedule = simulatePayoff card (Money 500) (Money 100)

      length schedule `shouldSatisfy` (> 0)
      length schedule `shouldSatisfy` (< 20)

    it "ends with zero balance" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.12
            , ccMinPayment = FixedAmount (Money 100)
            }
      let schedule = simulatePayoff card (Money 500) (Money 100)

      case schedule of
        [] -> expectationFailure "Schedule should not be empty"
        _ -> unMoney (pmBalance (last schedule)) `shouldSatisfy` (< 0.01)

  describe "Edge cases" $ do
    it "handles zero balance" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = FixedAmount (Money 25)
            }
      let stmt = analyzeBalance card (Money 0)

      stmtPayoffMonths stmt `shouldBe` Just 0
      unMoney (stmtMinPayment stmt) `shouldBe` 0

    it "handles small balance less than fixed minimum" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = FixedAmount (Money 100)
            }
      let minPmt = minimumPayment card (Money 50)

      unMoney minPmt `shouldBe` 50  -- Should be capped at balance

  -- Test cases from IMPLEMENTATION_PLAN.md
  describe "Implementation Plan Test Cases" $ do
    it "Credit card min: $1000, 18%, 2% min -> Min = $20" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.18
            , ccMinPayment = PercentOfBalance (Rate 0.02)
            }
      let minPmt = minimumPayment card (Money 1000)
      unMoney minPmt `shouldBe` 20

    it "Credit card payoff: $1000, 18%, $50/mo -> approximately 24 months" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.18
            , ccMinPayment = FixedAmount (Money 50)
            }
      let proj = payoffProjection card (Money 1000) (Money 50)
      -- Should be around 24 months (within reasonable range)
      ppMonths proj `shouldSatisfy` (\x -> x >= 20 && x <= 28)

  -- Additional edge case investigations
  describe "Edge Case Investigations" $ do
    it "Very high interest rate: 50% APR" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.50  -- 50% APR
            , ccMinPayment = FixedAmount (Money 100)
            }
      let proj = payoffProjection card (Money 1000) (Money 100)
      -- Should still pay off eventually
      ppMonths proj `shouldSatisfy` (> 0)
      -- Final balance in schedule should be zero
      let schedule = simulatePayoff card (Money 1000) (Money 100)
      case schedule of
        [] -> expectationFailure "Should have payoff schedule"
        _ -> unMoney (pmBalance (last schedule)) `shouldSatisfy` (< 0.01)

    it "Very small balance: $1" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.18
            , ccMinPayment = FixedAmount (Money 25)
            }
      let minPmt = minimumPayment card (Money 1)
      unMoney minPmt `shouldBe` 1  -- Capped at balance

    it "Large balance: $100,000" $ do
      let card = CreditCard
            { ccLimit = Money 200000
            , ccAnnualRate = Rate 0.22
            , ccMinPayment = GreaterOf (Money 100) (Rate 0.02)
            }
      let stmt = analyzeBalance card (Money 100000)
      -- Minimum should be 2% = $2000
      unMoney (stmtMinPayment stmt) `shouldBe` 2000
      -- Should have payoff months
      stmtPayoffMonths stmt `shouldSatisfy` (/= Nothing)

    it "Payment exactly covers interest (infinite payoff)" $ do
      -- If payment only covers interest, balance never decreases
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.12  -- 1% monthly
            , ccMinPayment = FixedAmount (Money 10)  -- $10 payment on $1000 = 1% = interest only
            }
      -- With $1000 balance and $10 payment, interest is exactly $10
      -- So principal never decreases - should detect this
      let schedule = simulatePayoff card (Money 1000) (Money 10)
      -- Should return empty schedule (can't pay off)
      length schedule `shouldBe` 0

    it "Payment less than interest (impossible payoff)" $ do
      let card = CreditCard
            { ccLimit = Money 10000
            , ccAnnualRate = Rate 0.24  -- 2% monthly
            , ccMinPayment = FixedAmount (Money 10)
            }
      -- With $1000 balance and $10 payment, interest is $20 > payment
      let schedule = simulatePayoff card (Money 1000) (Money 10)
      -- Should return empty schedule (can't pay off)
      length schedule `shouldBe` 0
