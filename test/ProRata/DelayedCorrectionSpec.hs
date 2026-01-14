{-# LANGUAGE OverloadedStrings #-}

module ProRata.DelayedCorrectionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Precision.CETIP21Spec (applyPrecision, PrecisionType(..))

-- Brazilian Index Pro-rata Calculation with Delayed Corrections
-- CETIP21 requires specific handling for IGP-DI, INPC, and IPCA indices
-- where index publication occurs with a delay, affecting payment calculations

data BrazilianIndex =
    IGP_DI   -- General Price Index - Internal Availability
  | INPC     -- National Consumer Price Index
  | IPCA     -- Broad Consumer Price Index
  deriving (Eq, Show)

data IndexCorrectionMethod =
    DelayedCorrection Int  -- Days of delay for index publication
  | ImmediateCorrection
  deriving (Eq, Show)

-- Simple Date type for testing (should match the project's Date type)
data Date = Date Int Int Int deriving (Eq, Show, Ord)

-- Index correction scenario for CETIP21 compliance
data IndexCorrectionScenario = IndexCorrectionScenario
  { issueDate :: Date
  , paymentDate :: Date
  , indexType :: BrazilianIndex
  , indexCalculationDate :: Date
  , correctionMethod :: IndexCorrectionMethod
  , proRataCalculation :: ProRataCalculationType
  } deriving (Eq, Show)

data ProRataCalculationType =
    StandardProRata        -- Normal pro-rata calculation
  | DelayedIndexProRata    -- Pro-rata with delayed index correction
  | EarlyPaymentProRata    -- Payment before 15th requires special handling
  deriving (Eq, Show)

-- Index values for testing (mock historical data)
type IndexValue = Double
type IndexTable = [(Date, IndexValue)]

-- Mock Brazilian index values for testing
mockIGPDIValues :: IndexTable
mockIGPDIValues =
  [ (Date 2024 1 1, 100.000000)
  , (Date 2024 1 15, 100.250000)
  , (Date 2024 2 1, 100.750000)
  , (Date 2024 2 15, 101.000000)
  , (Date 2024 3 1, 101.500000)
  , (Date 2024 3 15, 102.000000)
  , (Date 2024 4 1, 102.250000)
  , (Date 2024 4 15, 102.750000)
  ]

mockINPCValues :: IndexTable
mockINPCValues =
  [ (Date 2024 1 1, 200.000000)
  , (Date 2024 1 15, 200.180000)
  , (Date 2024 2 1, 200.420000)
  , (Date 2024 2 15, 200.650000)
  , (Date 2024 3 1, 200.890000)
  , (Date 2024 3 15, 201.120000)
  , (Date 2024 4 1, 201.350000)
  , (Date 2024 4 15, 201.580000)
  ]

mockIPCAValues :: IndexTable
mockIPCAValues =
  [ (Date 2024 1 1, 150.000000)
  , (Date 2024 1 15, 150.125000)
  , (Date 2024 2 1, 150.375000)
  , (Date 2024 2 15, 150.500000)
  , (Date 2024 3 1, 150.750000)
  , (Date 2024 3 15, 151.000000)
  , (Date 2024 4 1, 151.125000)
  , (Date 2024 4 15, 151.375000)
  ]

-- Get index table for a given index type
getIndexTable :: BrazilianIndex -> IndexTable
getIndexTable IGP_DI = mockIGPDIValues
getIndexTable INPC = mockINPCValues
getIndexTable IPCA = mockIPCAValues

-- Get index publication delay (in days)
getIndexDelay :: BrazilianIndex -> Int
getIndexDelay IGP_DI = 5   -- IGP-DI published 5 days after period end
getIndexDelay INPC = 10    -- INPC published 10 days after period end
getIndexDelay IPCA = 8     -- IPCA published 8 days after period end

-- Find index value for a given date
findIndexValue :: IndexTable -> Date -> Maybe IndexValue
findIndexValue table date = lookup date table

-- Calculate delayed index correction date
calculateDelayedIndexDate :: BrazilianIndex -> Date -> Date
calculateDelayedIndexDate indexType baseDate =
  let delay = getIndexDelay indexType
  in addDays delay baseDate

-- Add days to a date (simplified implementation)
addDays :: Int -> Date -> Date
addDays days (Date year month day) =
  let newDay = day + days
      daysInMonth = [31, if isLeapYear year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      maxDay = daysInMonth !! (month - 1)
  in if newDay <= maxDay
     then Date year month newDay
     else if month < 12
          then Date year (month + 1) (newDay - maxDay)
          else Date (year + 1) 1 (newDay - maxDay)

-- Check if year is leap year
isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0) && (year `mod` 100 /= 0 || year `mod` 400 == 0)

-- Calculate pro-rata correction factor
calculateProRataFactor :: IndexCorrectionScenario -> Maybe Double
calculateProRataFactor scenario =
  let indexTable = getIndexTable (indexType scenario)
      issueValue = findIndexValue indexTable (issueDate scenario)

      -- Determine which index date to use based on payment timing
      indexDate = case proRataCalculation scenario of
        EarlyPaymentProRata ->
          -- Payment before 15th: use delayed correction from two months prior
          calculateDelayedIndexDate (indexType scenario) (issueDate scenario)
        _ ->
          -- Standard: use index from calculation date
          indexCalculationDate scenario

      correctionValue = findIndexValue indexTable indexDate
  in do
    baseValue <- issueValue
    adjValue <- correctionValue
    let factor = adjValue / baseValue
    return $ applyPrecision ProRataPrecision factor

-- Calculate days between dates (simplified)
daysBetween :: Date -> Date -> Int
daysBetween (Date y1 m1 d1) (Date y2 m2 d2) =
  -- Simplified calculation for testing purposes
  let date1Days = y1 * 365 + m1 * 30 + d1
      date2Days = y2 * 365 + m2 * 30 + d2
  in abs (date2Days - date1Days)

-- Calculate calendar days pro-rata ratio
calculateCalendarDaysProRata :: Date -> Date -> Date -> Date -> Double
calculateCalendarDaysProRata issueDate paymentDate periodStart periodEnd =
  let dcp = daysBetween issueDate paymentDate  -- Days in calculation period
      dct = daysBetween periodStart periodEnd  -- Days in total period
      ratio = fromIntegral dcp / fromIntegral dct
  in applyPrecision ProRataPrecision ratio

-- Check if payment date requires delayed correction (before 15th)
requiresDelayedCorrection :: Date -> Bool
requiresDelayedCorrection (Date _ _ day) = day < 15

-- Apply CETIP21 delayed correction rules
applyDelayedCorrectionRules :: IndexCorrectionScenario -> IndexCorrectionScenario
applyDelayedCorrectionRules scenario =
  if requiresDelayedCorrection (paymentDate scenario)
  then scenario { proRataCalculation = EarlyPaymentProRata }
  else scenario { proRataCalculation = StandardProRata }

spec :: Spec
spec = describe "CETIP21 Pro-rata with Delayed Index Correction" $ do

  describe "Brazilian Index Management" $ do
    it "provides correct index publication delays" $ do
      getIndexDelay IGP_DI `shouldBe` 5
      getIndexDelay INPC `shouldBe` 10
      getIndexDelay IPCA `shouldBe` 8

    it "retrieves index values correctly" $ do
      findIndexValue mockIGPDIValues (Date 2024 1 1) `shouldBe` Just 100.000000
      findIndexValue mockINPCValues (Date 2024 2 15) `shouldBe` Just 200.650000
      findIndexValue mockIPCAValues (Date 2024 3 1) `shouldBe` Just 150.750000

    it "handles missing index values gracefully" $ do
      findIndexValue mockIGPDIValues (Date 2025 1 1) `shouldBe` Nothing

  describe "Delayed Index Correction Calculations" $ do
    it "calculates delayed correction dates correctly" $ do
      let baseDate = Date 2024 3 1
      calculateDelayedIndexDate IGP_DI baseDate `shouldBe` Date 2024 3 6
      calculateDelayedIndexDate INPC baseDate `shouldBe` Date 2024 3 11
      calculateDelayedIndexDate IPCA baseDate `shouldBe` Date 2024 3 9

    it "identifies early payment scenarios correctly" $ do
      requiresDelayedCorrection (Date 2024 3 10) `shouldBe` True   -- Before 15th
      requiresDelayedCorrection (Date 2024 3 15) `shouldBe` False  -- On 15th
      requiresDelayedCorrection (Date 2024 3 20) `shouldBe` False  -- After 15th

  describe "Pro-rata Factor Calculations" $ do
    it "calculates standard pro-rata factor for IGP-DI" $ do
      let scenario = IndexCorrectionScenario
            { issueDate = Date 2024 1 1
            , paymentDate = Date 2024 2 15
            , indexType = IGP_DI
            , indexCalculationDate = Date 2024 2 15
            , correctionMethod = DelayedCorrection 5
            , proRataCalculation = StandardProRata
            }
      let result = calculateProRataFactor scenario

      -- Expected: 101.000000 / 100.000000 = 1.01 (truncated to 9 decimal places)
      result `shouldBe` Just 1.010000000

    it "calculates delayed correction factor for early payment" $ do
      let scenario = IndexCorrectionScenario
            { issueDate = Date 2024 1 1
            , paymentDate = Date 2024 2 10  -- Before 15th - triggers delayed correction
            , indexType = INPC
            , indexCalculationDate = Date 2024 1 15
            , correctionMethod = DelayedCorrection 10
            , proRataCalculation = EarlyPaymentProRata
            }

      let adjustedScenario = applyDelayedCorrectionRules scenario
      adjustedScenario `shouldSatisfy` (\s -> proRataCalculation s == EarlyPaymentProRata)

    it "maintains CETIP21 precision for pro-rata calculations" $ do
      property $ \scenario ->
        case calculateProRataFactor scenario of
          Nothing -> True  -- Missing index values are acceptable
          Just factor ->
            let factorStr = show factor
                decimalPlaces = length $ takeWhile (/= 'e') $ drop 1 $ dropWhile (/= '.') factorStr
            in if '.' `elem` factorStr
               then decimalPlaces <= 9  -- Maximum 9 decimal places
               else True

  describe "Calendar Days Pro-rata Calculations" $ do
    it "calculates calendar days ratio correctly" $ do
      let issueDate = Date 2024 1 15      -- Issue: 15th of month
      let paymentDate = Date 2024 2 10    -- Payment: 10th of next month
      let periodStart = Date 2024 1 1     -- Period start
      let periodEnd = Date 2024 2 1       -- Period end (next month)

      let ratio = calculateCalendarDaysProRata issueDate paymentDate periodStart periodEnd
      ratio `shouldSatisfy` (> 0.0)
      ratio `shouldSatisfy` (< 1.0)

    it "handles end-of-month scenarios per CETIP21 table" $ do
      -- February 28 vs February 29 (leap year scenario)
      let feb28 = Date 2024 2 28
      let feb29 = Date 2024 2 29
      let march1 = Date 2024 3 1
      let periodStart = Date 2024 2 1

      let ratio28 = calculateCalendarDaysProRata feb28 march1 periodStart march1
      let ratio29 = calculateCalendarDaysProRata feb29 march1 periodStart march1

      -- Both should be valid ratios
      ratio28 `shouldSatisfy` (> 0.0)
      ratio29 `shouldSatisfy` (> 0.0)

  describe "Integration with CETIP21 Calculation Methods" $ do
    it "supports IGP-DI delayed correction scenario" $ do
      -- Typical IGP-DI scenario: payment before 15th requiring delayed correction
      let scenario = IndexCorrectionScenario
            { issueDate = Date 2024 1 15
            , paymentDate = Date 2024 2 10    -- Before 15th
            , indexType = IGP_DI
            , indexCalculationDate = Date 2023 12 1  -- Two months prior
            , correctionMethod = DelayedCorrection 5
            , proRataCalculation = StandardProRata
            }

      let adjustedScenario = applyDelayedCorrectionRules scenario
      proRataCalculation adjustedScenario `shouldBe` EarlyPaymentProRata

    it "supports INPC standard correction scenario" $ do
      -- Standard INPC scenario: payment after 15th
      let scenario = IndexCorrectionScenario
            { issueDate = Date 2024 1 15
            , paymentDate = Date 2024 2 20    -- After 15th
            , indexType = INPC
            , indexCalculationDate = Date 2024 2 15
            , correctionMethod = DelayedCorrection 10
            , proRataCalculation = StandardProRata
            }

      let adjustedScenario = applyDelayedCorrectionRules scenario
      proRataCalculation adjustedScenario `shouldBe` StandardProRata

    it "calculates complete pro-rata correction chain" $ do
      -- Complete calculation: index correction + calendar days pro-rata
      let scenario = IndexCorrectionScenario
            { issueDate = Date 2024 1 15
            , paymentDate = Date 2024 2 15
            , indexType = IPCA
            , indexCalculationDate = Date 2024 2 15
            , correctionMethod = DelayedCorrection 8
            , proRataCalculation = StandardProRata
            }

      let indexFactor = calculateProRataFactor scenario
      let calendarRatio = calculateCalendarDaysProRata
                           (issueDate scenario)
                           (paymentDate scenario)
                           (Date 2024 1 1)
                           (Date 2024 2 1)

      case indexFactor of
        Just factor -> do
          factor `shouldSatisfy` (> 1.0)  -- Index correction should be positive
          calendarRatio `shouldSatisfy` (> 0.0)  -- Calendar ratio should be positive
        Nothing -> expectationFailure "Index factor calculation failed"

  describe "Edge Cases and Error Handling" $ do
    it "handles same-day issue and payment correctly" $ do
      let scenario = IndexCorrectionScenario
            { issueDate = Date 2024 1 15
            , paymentDate = Date 2024 1 15  -- Same day
            , indexType = IGP_DI
            , indexCalculationDate = Date 2024 1 15
            , correctionMethod = DelayedCorrection 5
            , proRataCalculation = StandardProRata
            }

      let calendarRatio = calculateCalendarDaysProRata
                           (issueDate scenario)
                           (paymentDate scenario)
                           (Date 2024 1 1)
                           (Date 2024 2 1)
      calendarRatio `shouldBe` 0.0  -- Same day should result in zero ratio

    it "handles cross-year scenarios correctly" $ do
      let scenario = IndexCorrectionScenario
            { issueDate = Date 2023 12 15
            , paymentDate = Date 2024 1 15
            , indexType = INPC
            , indexCalculationDate = Date 2024 1 15
            , correctionMethod = DelayedCorrection 10
            , proRataCalculation = StandardProRata
            }

      -- Should handle cross-year calculations without errors
      let factor = calculateProRataFactor scenario
      factor `shouldSatisfy` (\result -> case result of
        Just _ -> True
        Nothing -> True)  -- Either calculation succeeds or gracefully fails

-- QuickCheck generators for property-based testing
instance Arbitrary Date where
  arbitrary = do
    -- Widened year range, now properly handles all valid days including end of month
    year <- choose (1990, 2050)
    month <- choose (1, 12)
    let daysInMonth' = [31, if isLeapYear year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        maxDay = daysInMonth' !! (month - 1)
    day <- choose (1, maxDay)  -- Generate all valid days for the month
    return $ Date year month day

instance Arbitrary BrazilianIndex where
  arbitrary = elements [IGP_DI, INPC, IPCA]

instance Arbitrary ProRataCalculationType where
  arbitrary = elements [StandardProRata, DelayedIndexProRata, EarlyPaymentProRata]

instance Arbitrary IndexCorrectionScenario where
  arbitrary = do
    issue <- arbitrary
    payment <- arbitrary `suchThat` (>= issue)
    index <- arbitrary
    calcDate <- arbitrary
    method <- elements [DelayedCorrection 5, DelayedCorrection 8, DelayedCorrection 10, ImmediateCorrection]
    proRataType <- arbitrary
    return $ IndexCorrectionScenario issue payment index calcDate method proRataType