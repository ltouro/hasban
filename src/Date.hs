{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Date
  ( Date(..)
  , Frequency(..)
  , toDate
  , numberOfDays
  , numberOfMonths
  , formatDate
  , addTerm
  , fixDate
  , day
  , month
  , year
  , numberOfWeekdays
  , previousMonthAniversary
  , nextMonthAniversary
  ) where

import           Data.Aeson         (FromJSON (parseJSON), ToJSON (toJSON),
                                     Value (String), withText)
import qualified Data.Text          as T
import           Data.Time          (defaultTimeLocale, diffDays, fromGregorian,
                                     toGregorian, Day)
import           Data.Time.Calendar (addDays, addGregorianMonthsClip,
                                     addGregorianYearsClip)
import           Data.Time.Format   (parseTimeM)
import           GHC.Generics       (Generic)
import           Text.Printf        (printf)
import           Data.Time.Calendar.WeekDate (toWeekDate)

data Date = Date Integer Int Int
  deriving (Show, Read, Eq, Ord)

data Frequency =
  Monthly |
  Biweekly |
  Weekly |
  Daily |
  Yearly
  deriving (Enum, Show, Generic, Eq)
instance FromJSON Frequency
instance ToJSON Frequency

day :: Date -> Int
day (Date _ _ i) = i
month :: Date -> Int
month (Date _ i _) = i
year :: Date -> Integer
year (Date i _ _) = i

toDate :: (Integer, Int, Int) -> Date
toDate (nextYear, nextMonth, nextDay ) = Date nextYear nextMonth nextDay

instance ToJSON Date where
  toJSON (Date y m d) = String $ T.pack $ printf "%04d-%02d-%02d" y m d

instance FromJSON Date where
  parseJSON = withText "date" $ \t ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
      Nothing -> fail "Invalid date format"
      Just d  -> return $ toDate $ toGregorian d


numberOfDays :: Date -> Date -> Integer
numberOfDays  (Date year1 month1 day1) (Date year2 month2 day2) =
  let date1 = fromGregorian year1 month1 day1
      date2 = fromGregorian year2 month2 day2
  in  diffDays date2 date1

numberOfMonths :: Date -> Date -> Integer
numberOfMonths (Date y1 m1 d1) (Date y2 m2 d2) =
  let
    yearDiff = fromIntegral (y2 - y1)
    monthDiff = fromIntegral (m2 - m1)
    totalMonths = 12 * yearDiff + monthDiff
    dayDiff = d2 - d1
  in if dayDiff >= 0
     then totalMonths
     else totalMonths - 1

dateToDay :: Date -> Day
dateToDay (Date year month day) = fromGregorian year month day

isWeekday :: Day -> Bool
isWeekday day =
  let (_, _, weekday) = toWeekDate day
  in weekday /= 6 && weekday /= 7

numberOfWeekdays' :: Day -> Day -> Int
numberOfWeekdays' startDay endDay =
  let allDays = [startDay .. endDay]
  in fromIntegral . length $ filter isWeekday allDays

numberOfWeekdays :: Date -> Date -> Int
numberOfWeekdays d1 d2 = numberOfWeekdays' (dateToDay d1)  (dateToDay d2)

formatDate :: Date -> String
formatDate (Date year month day) =
  show year ++ "-" ++ printf "%02d" month ++ "-" ++ printf "%02d" day

-- return the date added of given number of periods of certain frequency
addTerm :: Date -> Frequency -> Int -> Date
addTerm (Date year month day) paymentFrequency' numPeriods =
  let current = fromGregorian year month day
      next    = case paymentFrequency' of
        Monthly  -> addGregorianMonthsClip (fromIntegral numPeriods) current
        Yearly   -> addGregorianYearsClip (fromIntegral numPeriods) current
        Biweekly -> addDays (fromIntegral numPeriods * 14) current
        Weekly   -> addDays (fromIntegral numPeriods * 7) current
        Daily    -> addDays (fromIntegral numPeriods) current
  in  toDate $ toGregorian next

fixDate :: Date -> Int -> Date
fixDate (Date year month day) dayOfMonth
  | day < dayOfMonth = Date year month dayOfMonth
  | day > dayOfMonth = if month == 12
                          then Date (year + 1) 1 dayOfMonth
                          else Date year (month + 1) dayOfMonth
  | otherwise = Date year month day

previousMonthAniversary :: Date -> Int -> Date
previousMonthAniversary (Date year month day) d =
  let previousYear  = if day < d && month == 1 then year - 1 else year
      previousMonth = if day < d then if month == 1 then 12 else month - 1 else month
  in Date previousYear previousMonth d

nextMonthAniversary :: Date -> Int -> Date
nextMonthAniversary (Date year month day) d =
  let nextYear  = if (day > d) && month == 12 then year + 1 else year
      nextMonth = if day > d then if month == 12 then 1 else month + 1 else month
  in Date nextYear nextMonth d