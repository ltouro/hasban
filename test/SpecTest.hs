module Main where

import Test.Hspec

import Date ( Frequency(Monthly), Date(Date) )
import LoanCalculator(
  LoanQuery (..)
  , Commission (..)
  , Frequency (..)
  , LoanQuery
  , CommissionType(Absolute, Percentage)
  , AmortizationType(SAC, PRICE)
  , queryLoan
  , asLiquidValueLoan
  , LoanQueryResult (..)
  , AmortizationTable (..)
  , AmortizationTableItem (..)
  )

main :: IO ()
main = hspec modelSpec

modelSpec :: Spec
modelSpec = describe "Tests of amortization schedule calculation" $ do

  context "PRICE amortization schedule" $
    it "calculate liquid value" $
    resultLiquidValue (queryLoan LoanQuery {
      loanAmount = 31500,
      interestRate = 0.047,
      loanTerms = 6,
      paymentFrequency = Monthly,
      startDate = Date 2022 02 28,
      gracePeriodInterestTerms = 0,
      gracePeriodPrincipalTerms = 2,
      commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
      amortizationType = PRICE
  }) `shouldBe` 29549.709936719013

  context "SAC amortization schedule" $
    it "calculate liquid value" $
    resultLiquidValue (queryLoan LoanQuery {
      loanAmount = 31500,
      interestRate = 0.047,
      loanTerms = 6,
      paymentFrequency = Monthly,
      startDate = Date 2022 02 28,
      gracePeriodInterestTerms = 0,
      gracePeriodPrincipalTerms = 2,
      commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
      amortizationType = SAC
  }) `shouldBe` 29554.245

  context "SAC amortization schedule" $
    it "can balance query by liquid value" $ do
      let loanQuery = LoanQuery
            { loanAmount = 31500,
              interestRate = 0.047,
              loanTerms = 6,
              paymentFrequency = Monthly,
              startDate = Date 2022 02 28,
              gracePeriodInterestTerms = 0,
              gracePeriodPrincipalTerms = 2,
              commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
              amortizationType = SAC
            }
      let expectedValue = 31500
      let actualValue = round $ resultLiquidValue $ queryLoan $ asLiquidValueLoan loanQuery
      actualValue `shouldBe` expectedValue

  context "PRICE amortization schedule" $
    it "can balance query by liquid value" $ do
      let expectedValue = 31500
      let loanQuery = LoanQuery
            { loanAmount = expectedValue,
              interestRate = 0.047,
              loanTerms = 6,
              paymentFrequency = Monthly,
              startDate = Date 2022 02 28,
              gracePeriodInterestTerms = 0,
              gracePeriodPrincipalTerms = 2,
              commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
              amortizationType = PRICE
            }
      let actualValue = fromIntegral $ round $ resultLiquidValue $ queryLoan $ asLiquidValueLoan loanQuery
      actualValue `shouldBe` expectedValue 

  context "PRICE amortization schedule" $
    it "last payment principal very close to 0" $ do
      let expectedValue = 31500
      let loanQuery = LoanQuery
            { loanAmount = expectedValue,
              interestRate = 0.047,
              loanTerms = 6,
              paymentFrequency = Monthly,
              startDate = Date 2022 02 28,
              gracePeriodInterestTerms = 0,
              gracePeriodPrincipalTerms = 2,
              commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
              amortizationType = PRICE
            }      
      let roundedLastRemainingPrincipal = fromIntegral $ round $ remainingPrincipal $ last $ paymentItems $ resultSchedule (queryLoan $ asLiquidValueLoan loanQuery)
      roundedLastRemainingPrincipal `shouldBe` 0

  context "SAC amortization schedule" $
    it "last payment principal very close to 0" $ do
      let expectedValue = 500000
      let loanQuery = LoanQuery
            { loanAmount = expectedValue,
              interestRate = 0.047,
              loanTerms = 60,
              paymentFrequency = Monthly,
              startDate = Date 2022 02 28,
              gracePeriodInterestTerms = 0,
              gracePeriodPrincipalTerms = 2,
              commissions = [Commission Percentage 2.0, Commission Percentage 2.5],
              amortizationType = SAC
            }      
      let roundedLastRemainingPrincipal = fromIntegral $ round $ remainingPrincipal $ last $ paymentItems $ resultSchedule (queryLoan $ asLiquidValueLoan loanQuery)
      roundedLastRemainingPrincipal `shouldBe` 0