{-# LANGUAGE DeriveGeneric #-}

module Date
  ( Date(..)
  , Frequency(..)
  , toDate
  , numberOfDays
  , formatDate
  , addTerm
  ) where

import Data.Aeson
    ( FromJSON(parseJSON), Value(String), ToJSON(toJSON), withText )
import           Text.Printf            (printf)
import qualified Data.Text as T
import Data.Time
    ( defaultTimeLocale, toGregorian, fromGregorian, diffDays )
import Data.Time.Format (parseTimeM)
import GHC.Generics (Generic)
import Data.Time.Calendar (addGregorianMonthsClip, addGregorianYearsClip, addDays)

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

toDate :: (Integer, Int, Int) -> Date
toDate (nextYear, nextMonth, nextDay ) = Date nextYear nextMonth nextDay

instance ToJSON Date where
  toJSON (Date y m d) = String $ T.pack $ printf "%04d-%02d-%02d" y m d

instance FromJSON Date where
  parseJSON = withText "date" $ \t ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
      Nothing -> fail "Invalid date format"
      Just d -> return $ toDate $ toGregorian d


numberOfDays :: Date -> Date -> Integer
numberOfDays  (Date year1 month1 day1) (Date year2 month2 day2) =
  let date1 = fromGregorian year1 month1 day1
      date2 = fromGregorian year2 month2 day2
  in  diffDays date2 date1


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
