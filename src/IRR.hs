{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module IRR
  ( calculateIrr
  ) where
import           Data.List (foldl')
import           Data.Time (Day, diffDays, fromGregorian)
import           Date

data CashFlowDates where
  CashFlowDates :: {amount :: Double, date :: Day} -> CashFlowDates
    deriving (Show)

data CashFlowFractionOfYear where
  CashFlowFractionOfYear :: {amount' :: Double, years :: Double}
                            -> CashFlowFractionOfYear
    deriving (Show)

defaultPrecision :: Double
defaultPrecision = 10 ** (-12)

minRate :: Double
minRate = -1 + (defaultPrecision * 10)

maxRate :: Double
maxRate = 1000.00

calculateIrr :: [(Date, Double)] -> Maybe Double
calculateIrr cashFlow =
  let
    cashFlowDates = map (\c -> CashFlowDates (snd c) (dateToDay $ fst c)) cashFlow
  in calculate cashFlowDates minRate maxRate defaultPrecision

toFractionOfYears :: [CashFlowDates] -> [CashFlowFractionOfYear]
toFractionOfYears cashflows =
  let
    firstDate = minimum $ map date cashflows
    diffInDays cf = fromIntegral (diffDays (date cf) firstDate)
  in
    map (\cf -> CashFlowFractionOfYear (amount cf) (diffInDays cf / 365)) cashflows

calcEquation :: [CashFlowFractionOfYear] -> Double -> Double
calcEquation cashflows interestRate =
  foldl' (\acc cf -> acc + (amount' cf / ((1 + interestRate) ** years cf))) 0 cashflows

calculate :: [CashFlowDates] -> Double -> Double -> Double -> Maybe Double
calculate cashflows lowRate highRate prescision = let
      cashFlowYears = toFractionOfYears cashflows
      f = calcEquation cashFlowYears
    in bisection f lowRate highRate prescision

-- ref: https://gist.github.com/PhDP/5849881
bisection :: (Double -> Double) -> Double -> Double -> Double -> Maybe Double
bisection f a b err
  | err < 1e-15           = Nothing
  | abs (b - a) / 2 < err = Nothing
  | a < b                 = bis a b
  | otherwise             = bis b a
  where
    bis a b
      | d < err || f m == 0.0 = Just m
      | f a * f m < 0.0       = bis a m
      | f m * f b < 0.0       = bis m b
      | otherwise             = Nothing
      where
        d = (b - a) / 2
        m = (b + a) / 2

exampleCashflow :: [CashFlowDates]
exampleCashflow =
  [ CashFlowDates { amount = -10000, date = fromGregorian 2023 1 1 }
  , CashFlowDates { amount = 12000, date = fromGregorian 2024 1 1  }
  ]
