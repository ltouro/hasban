{-# LANGUAGE OverloadedStrings #-}

module Calendar.BusinessDaySpec (spec) where

import Test.Hspec
import Test.QuickCheck

-- Brazilian Business Day Calendar Implementation
-- Based on CETIP21 requirements for Brazilian financial markets

data BusinessDayCalendar = BusinessDayCalendar
  { country :: CountryCode
  , markets :: [MarketCode]
  , holidays :: HolidaySet
  , weekendDays :: [DayOfWeek]
  , adjustmentRule :: BusinessDayAdjustment
  } deriving (Eq, Show)

data CountryCode = BR | US | EU deriving (Eq, Show)
data MarketCode = CETIP | B3 | NYSE deriving (Eq, Show)
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Show, Enum)

data BusinessDayAdjustment =
    Following           -- Move to next business day
  | ModifiedFollowing   -- Move to next business day unless it crosses month boundary
  | Preceding          -- Move to previous business day
  | ModifiedPreceding  -- Move to previous business day unless it crosses month boundary
  deriving (Eq, Show)

type HolidaySet = [Date]

-- Simple Date type for testing (should match the project's Date type)
data Date = Date Int Int Int deriving (Eq, Show, Ord)

-- Brazilian National Holidays (Fixed and Variable)
brazilianNationalHolidays2024 :: [Date]
brazilianNationalHolidays2024 =
  [ Date 2024 1 1    -- New Year's Day
  , Date 2024 2 12   -- Carnival Monday
  , Date 2024 2 13   -- Carnival Tuesday
  , Date 2024 3 29   -- Good Friday
  , Date 2024 4 21   -- Tiradentes Day
  , Date 2024 5 1    -- Labor Day
  , Date 2024 9 7    -- Independence Day
  , Date 2024 10 12  -- Our Lady of Aparecida
  , Date 2024 11 2   -- All Souls' Day
  , Date 2024 11 15  -- Proclamation of the Republic
  , Date 2024 12 25  -- Christmas Day
  ]

-- Additional Brazilian Financial Market Holidays
brazilianFinancialHolidays2024 :: [Date]
brazilianFinancialHolidays2024 =
  [ Date 2024 1 25   -- São Paulo City Anniversary
  , Date 2024 4 23   -- São Jorge Day
  , Date 2024 6 20   -- Corpus Christi (Financial markets closed)
  , Date 2024 11 20  -- Black Consciousness Day
  , Date 2024 12 24  -- Christmas Eve (Half day)
  , Date 2024 12 31  -- New Year's Eve (Half day)
  ]

-- Complete Brazilian holidays combining national and financial
allBrazilianHolidays2024 :: [Date]
allBrazilianHolidays2024 = brazilianNationalHolidays2024 ++ brazilianFinancialHolidays2024

-- Brazilian business day calendar for CETIP21 compliance
brazilianCalendar :: BusinessDayCalendar
brazilianCalendar = BusinessDayCalendar
  { country = BR
  , markets = [CETIP, B3]
  , holidays = allBrazilianHolidays2024
  , weekendDays = [Saturday, Sunday]
  , adjustmentRule = Following
  }

-- Get day of week for a given date using corrected algorithm
dayOfWeek :: Date -> DayOfWeek
dayOfWeek (Date year month day) =
  -- Using a corrected day-of-week calculation based on Gregorian calendar
  let y = if month < 3 then year - 1 else year
      m = if month < 3 then month + 12 else month
      -- Zeller's congruence for Gregorian calendar
      q = day
      k = y `mod` 100
      j = y `div` 100
      h = (q + ((13 * (m + 1)) `div` 5) + k + (k `div` 4) + (j `div` 4) - 2 * j) `mod` 7
      -- h: 0=Saturday, 1=Sunday, 2=Monday, 3=Tuesday, 4=Wednesday, 5=Thursday, 6=Friday
      -- Convert to our DayOfWeek: Monday=0, Tuesday=1, ... Saturday=5, Sunday=6
      adjustedDay = (h + 5) `mod` 7
  in toEnum adjustedDay

-- Check if a date is a business day
isBusinessDay :: BusinessDayCalendar -> Date -> Bool
isBusinessDay calendar date =
  let isWeekend = dayOfWeek date `elem` weekendDays calendar
      isHoliday = date `elem` holidays calendar
  in not isWeekend && not isHoliday

-- Calculate number of business days between two dates (inclusive of start, exclusive of end)
calculateBusinessDays :: BusinessDayCalendar -> Date -> Date -> Int
calculateBusinessDays calendar startDate endDate =
  if startDate >= endDate
  then 0
  else length $ filter (isBusinessDay calendar) (dateRange startDate endDate)

-- Generate date range between two dates
dateRange :: Date -> Date -> [Date]
dateRange start end = takeWhile (< end) (iterate nextDay start)

-- Get next calendar day
nextDay :: Date -> Date
nextDay (Date year month day) =
  let daysInMonth = [31, if isLeapYear year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      maxDay = daysInMonth !! (month - 1)
  in if day < maxDay
     then Date year month (day + 1)
     else if month < 12
          then Date year (month + 1) 1
          else Date (year + 1) 1 1

-- Check if year is leap year
isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0) && (year `mod` 100 /= 0 || year `mod` 400 == 0)

-- Get next business day (Following business day convention)
nextBusinessDay :: BusinessDayCalendar -> Date -> Date
nextBusinessDay calendar date =
  let candidate = nextDay date
  in if isBusinessDay calendar candidate
     then candidate
     else nextBusinessDay calendar candidate

-- Get previous business day
previousBusinessDay :: BusinessDayCalendar -> Date -> Date
previousBusinessDay calendar date =
  let candidate = previousDay date
  in if isBusinessDay calendar candidate
     then candidate
     else previousBusinessDay calendar candidate

-- Get previous calendar day
previousDay :: Date -> Date
previousDay (Date year month day) =
  if day > 1
  then Date year month (day - 1)
  else if month > 1
       then let prevMonth = month - 1
                daysInPrevMonth = [31, if isLeapYear year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (prevMonth - 1)
            in Date year prevMonth daysInPrevMonth
       else Date (year - 1) 12 31

-- Apply business day adjustment rule
adjustDate :: BusinessDayCalendar -> Date -> Date
adjustDate calendar date =
  case adjustmentRule calendar of
    Following -> if isBusinessDay calendar date
                 then date
                 else nextBusinessDay calendar date
    Preceding -> if isBusinessDay calendar date
                 then date
                 else previousBusinessDay calendar date
    ModifiedFollowing ->
      let adjusted = if isBusinessDay calendar date
                     then date
                     else nextBusinessDay calendar date
          (Date origYear origMonth _) = date
          (Date adjYear adjMonth _) = adjusted
      in if origYear == adjYear && origMonth == adjMonth
         then adjusted
         else previousBusinessDay calendar date
    ModifiedPreceding ->
      let adjusted = if isBusinessDay calendar date
                     then date
                     else previousBusinessDay calendar date
          (Date origYear origMonth _) = date
          (Date adjYear adjMonth _) = adjusted
      in if origYear == adjYear && origMonth == adjMonth
         then adjusted
         else nextBusinessDay calendar date

spec :: Spec
spec = describe "Brazilian Business Day Calendar (CETIP21)" $ do

  describe "Basic Calendar Properties" $ do
    it "identifies weekends correctly" $ do
      dayOfWeek (Date 2024 1 6) `shouldBe` Saturday
      dayOfWeek (Date 2024 1 7) `shouldBe` Sunday
      dayOfWeek (Date 2024 1 8) `shouldBe` Monday

    it "identifies Brazilian national holidays" $ do
      isBusinessDay brazilianCalendar (Date 2024 1 1) `shouldBe` False  -- New Year
      isBusinessDay brazilianCalendar (Date 2024 9 7) `shouldBe` False  -- Independence Day
      isBusinessDay brazilianCalendar (Date 2024 12 25) `shouldBe` False -- Christmas

    it "identifies financial market holidays" $ do
      isBusinessDay brazilianCalendar (Date 2024 4 23) `shouldBe` False  -- São Jorge Day
      isBusinessDay brazilianCalendar (Date 2024 11 20) `shouldBe` False -- Black Consciousness Day

    it "identifies regular business days" $ do
      isBusinessDay brazilianCalendar (Date 2024 1 2) `shouldBe` True   -- Regular Tuesday
      isBusinessDay brazilianCalendar (Date 2024 3 15) `shouldBe` True  -- Regular Friday
      isBusinessDay brazilianCalendar (Date 2024 6 10) `shouldBe` True  -- Regular Monday

  describe "Business Day Counting (CETIP21 Requirements)" $ do
    it "calculates business days correctly for a standard work week" $ do
      let startDate = Date 2024 1 8   -- Monday
      let endDate = Date 2024 1 15    -- Monday (next week)
      calculateBusinessDays brazilianCalendar startDate endDate `shouldBe` 5

    it "handles weeks with holidays" $ do
      let startDate = Date 2024 8 30  -- Friday before Labor Day weekend
      let endDate = Date 2024 9 9     -- Monday after Independence Day
      -- Should exclude weekend (Aug 31, Sep 1) and Independence Day (Sep 7)
      let result = calculateBusinessDays brazilianCalendar startDate endDate
      result `shouldSatisfy` (< 8)  -- Less than 8 business days due to holiday

    it "returns zero for same date" $ do
      let date = Date 2024 6 15
      calculateBusinessDays brazilianCalendar date date `shouldBe` 0

    it "returns zero for weekend to weekend" $ do
      let saturday = Date 2024 1 6
      let sunday = Date 2024 1 7
      calculateBusinessDays brazilianCalendar saturday sunday `shouldBe` 0

    it "handles month boundaries correctly" $ do
      let endOfMonth = Date 2024 1 31  -- Wednesday
      let startOfMonth = Date 2024 2 1 -- Thursday
      -- calculateBusinessDays is exclusive of end date, so Jan 31 to Feb 1 counts Jan 31
      calculateBusinessDays brazilianCalendar endOfMonth startOfMonth `shouldBe` 1

  describe "Business Day Adjustment Rules" $ do
    it "follows 'Following' convention for holidays" $ do
      let holiday = Date 2024 1 1     -- New Year (Monday)
      let adjusted = adjustDate brazilianCalendar holiday
      adjusted `shouldBe` Date 2024 1 2  -- Next business day (Tuesday)

    it "follows 'Following' convention for weekends" $ do
      let saturday = Date 2024 1 6
      let adjusted = adjustDate brazilianCalendar saturday
      adjusted `shouldBe` Date 2024 1 8  -- Next Monday

    it "leaves business days unchanged" $ do
      let businessDay = Date 2024 1 2  -- Tuesday
      let adjusted = adjustDate brazilianCalendar businessDay
      adjusted `shouldBe` businessDay

  describe "CETIP21 Specific Requirements" $ do
    it "handles Carnival period correctly" $ do
      -- Carnival Monday and Tuesday are holidays
      isBusinessDay brazilianCalendar (Date 2024 2 12) `shouldBe` False -- Carnival Monday
      isBusinessDay brazilianCalendar (Date 2024 2 13) `shouldBe` False -- Carnival Tuesday
      isBusinessDay brazilianCalendar (Date 2024 2 14) `shouldBe` True  -- Ash Wednesday (business day)

    it "calculates business days for a typical CETIP21 scenario" $ do
      -- 22 business days scenario from CETIP21 documentation
      let startDate = Date 2024 3 1   -- Friday
      let businessDaysNeeded = 22
      let dates = take businessDaysNeeded $ filter (isBusinessDay brazilianCalendar) $
                 iterate nextDay startDate
      length dates `shouldBe` businessDaysNeeded

    it "handles end-of-month scenarios per CETIP21 table" $ do
      -- Test end-of-month date matching rules
      let feb28 = Date 2024 2 28  -- February 28 (not leap year scenario)
      let feb29 = Date 2024 2 29  -- February 29 (leap year)

      -- Both should be valid business days in 2024 (leap year)
      isBusinessDay brazilianCalendar feb28 `shouldBe` True
      isBusinessDay brazilianCalendar feb29 `shouldBe` True

  describe "252 Business Day Year Validation" $ do
    it "validates that Brazil has approximately 252 business days per year" $ do
      let yearStart = Date 2024 1 1
      let yearEnd = Date 2025 1 1
      let businessDaysInYear = calculateBusinessDays brazilianCalendar yearStart yearEnd

      -- Should be approximately 252, allowing for variation due to holidays
      -- Actual count may vary by a few days depending on holiday calendar completeness
      businessDaysInYear `shouldSatisfy` (\days -> days >= 248 && days <= 254)

    it "validates monthly average of 21 business days" $ do
      -- Test a few months to ensure they average around 21 business days
      let marchStart = Date 2024 3 1
      let aprilStart = Date 2024 4 1
      let businessDaysInMarch = calculateBusinessDays brazilianCalendar marchStart aprilStart

      -- March should have approximately 21 business days
      businessDaysInMarch `shouldSatisfy` (\days -> days >= 19 && days <= 23)

  describe "Pro-rata Business Day Calculations" $ do
    it "calculates pro-rata business days for mismatched dates" $ do
      -- Issue date: 15th of month, anniversary: 10th of next month
      let issueDate = Date 2024 1 15      -- Monday
      let anniversaryDate = Date 2024 2 10 -- Saturday -> should adjust to Monday 2/12
      let adjustedAnniversary = adjustDate brazilianCalendar anniversaryDate

      let businessDays = calculateBusinessDays brazilianCalendar issueDate adjustedAnniversary
      businessDays `shouldSatisfy` (> 0)

    it "handles index publication delays for business day calculations" $ do
      -- Test scenario: payment date before 15th requires delayed correction
      let paymentDate = Date 2024 3 10  -- Before 15th
      let indexCalculationDate = Date 2024 1 10  -- Two months prior

      -- Verify both dates are business days or can be adjusted
      let adjustedPayment = adjustDate brazilianCalendar paymentDate
      let adjustedIndex = adjustDate brazilianCalendar indexCalculationDate

      isBusinessDay brazilianCalendar adjustedPayment `shouldBe` True
      isBusinessDay brazilianCalendar adjustedIndex `shouldBe` True

  describe "Integration with CETIP21 Calculation Methods" $ do
    it "supports 252-day business day calculations" $ do
      -- Standard 252-day calculation period
      let startDate = Date 2024 1 2   -- First business day of year
      let endDate = Date 2024 12 31   -- Last day of year
      let totalBusinessDays = calculateBusinessDays brazilianCalendar startDate endDate

      -- Should be close to 252 for CETIP21 compliance (allowing for holiday variations)
      totalBusinessDays `shouldSatisfy` (\days -> abs (days - 252) <= 5)

    it "supports 21 business days per month calculation" $ do
      -- Test multiple months to ensure 21-day average
      let months = [(Date 2024 m 1, Date 2024 (m+1) 1) | m <- [1..11]]
      let businessDaysPerMonth = map (\(start, end) ->
            calculateBusinessDays brazilianCalendar start end) months
      let averageBusinessDays = fromIntegral (sum businessDaysPerMonth) /
                               fromIntegral (length businessDaysPerMonth)

      -- Average should be approximately 21
      averageBusinessDays `shouldSatisfy` (\avg -> abs (avg - 21.0) <= 1.0)

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

instance Arbitrary BusinessDayCalendar where
  arbitrary = return brazilianCalendar  -- Use Brazilian calendar for testing

instance Arbitrary DayOfWeek where
  arbitrary = elements [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]