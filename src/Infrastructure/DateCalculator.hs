{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.DateCalculator 
  ( FinancialDate(..)
  , PaymentSchedule(..)
  , mkFinancialDate
  , addPaymentPeriods
  , daysBetween
  , monthsBetween
  , businessDaysBetween
  , isWeekday
  , formatFinancialDate
  , parseFinancialDate
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, diffDays, fromGregorian, toGregorian)
import Data.Time.Calendar (addDays, addGregorianMonthsClip, addGregorianYearsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (parseTimeM)
import GHC.Generics (Generic)
import Text.Printf (printf)

import Domain.Types (PaymentFrequency(..), PaymentNumber)

-- | Wrapper around Day with financial domain semantics
newtype FinancialDate = FinancialDate Day
  deriving (Eq, Ord, Show, Read, Generic)

-- | Payment schedule configuration
data PaymentSchedule = PaymentSchedule
  { frequency       :: PaymentFrequency
  , periodsPerTerm  :: Int  -- e.g., 1 for monthly, 2 for bi-monthly
  } deriving (Eq, Show, Generic)

instance FromJSON PaymentSchedule
instance ToJSON PaymentSchedule

-- Smart constructor for FinancialDate
mkFinancialDate :: Integer -> Int -> Int -> Either String FinancialDate
mkFinancialDate year month day
  | month < 1 || month > 12 = Left "Month must be between 1 and 12"
  | day < 1 || day > daysInMonth year month = Left "Invalid day for the given month"
  | otherwise = Right $ FinancialDate (fromGregorian year month day)

-- JSON instances
instance ToJSON FinancialDate where
  toJSON (FinancialDate day) = 
    let (year, month, dayOfMonth) = toGregorian day
    in toJSON $ printf "%04d-%02d-%02d" year month dayOfMonth

instance FromJSON FinancialDate where
  parseJSON = withText "FinancialDate" $ \t ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
      Nothing -> fail "Invalid date format. Expected YYYY-MM-DD"
      Just day -> return $ FinancialDate day

-- | Add payment periods to a date based on payment schedule
addPaymentPeriods :: FinancialDate -> PaymentSchedule -> PaymentNumber -> FinancialDate
addPaymentPeriods (FinancialDate currentDay) schedule periods =
  let totalPeriods = fromIntegral periods * periodsPerTerm schedule
      nextDay = case frequency schedule of
        Monthly  -> addGregorianMonthsClip totalPeriods currentDay
        Yearly   -> addGregorianYearsClip totalPeriods currentDay
        Biweekly -> addDays (totalPeriods * 14) currentDay
        Weekly   -> addDays (totalPeriods * 7) currentDay
        Daily    -> addDays totalPeriods currentDay
  in FinancialDate nextDay

-- | Calculate days between two financial dates
daysBetween :: FinancialDate -> FinancialDate -> Integer
daysBetween (FinancialDate date1) (FinancialDate date2) = diffDays date2 date1

-- | Calculate months between two financial dates
monthsBetween :: FinancialDate -> FinancialDate -> Integer
monthsBetween (FinancialDate date1) (FinancialDate date2) =
  let (y1, m1, d1) = toGregorian date1
      (y2, m2, d2) = toGregorian date2
      yearDiff = fromIntegral (y2 - y1)
      monthDiff = fromIntegral (m2 - m1)
      totalMonths = 12 * yearDiff + monthDiff
      dayDiff = d2 - d1
  in if dayDiff >= 0 then totalMonths else totalMonths - 1

-- | Check if a date falls on a weekday (Monday-Friday)
isWeekday :: FinancialDate -> Bool
isWeekday (FinancialDate day) =
  let (_, _, weekday) = toWeekDate day
  in weekday <= 5

-- | Count business days between two dates
businessDaysBetween :: FinancialDate -> FinancialDate -> Int
businessDaysBetween (FinancialDate startDay) (FinancialDate endDay) =
  let allDays = [startDay .. endDay]
  in length $ filter (\day -> let (_, _, weekday) = toWeekDate day in weekday <= 5) allDays

-- | Format financial date for display
formatFinancialDate :: FinancialDate -> String
formatFinancialDate (FinancialDate day) =
  let (year, month, dayOfMonth) = toGregorian day
  in printf "%04d-%02d-%02d" year month dayOfMonth

-- | Parse financial date from string
parseFinancialDate :: String -> Either String FinancialDate
parseFinancialDate dateStr =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
    Nothing -> Left $ "Invalid date format: " ++ dateStr ++ ". Expected YYYY-MM-DD"
    Just day -> Right $ FinancialDate day

-- Helper function to get days in a month
daysInMonth :: Integer -> Int -> Int
daysInMonth year month
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | month `elem` [4, 6, 9, 11] = 30
  | isLeapYear year = 29
  | otherwise = 28

-- Helper function to check if a year is a leap year
isLeapYear :: Integer -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4 == 0 = True
  | otherwise = False