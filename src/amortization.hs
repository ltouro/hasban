{-# LANGUAGE InstanceSigs #-}

module Amortization where
import Data.Fixed (E6, Fixed, Centi)

data Periodicity = Month | Day | Year 
    deriving (Enum, Show)

data Currency = 
    Currency { 
        value :: Centi,
        symbol :: String
    } deriving (Show)
data Percentage = Float
    deriving (Show)
data Duration = Int
    deriving (Show)

data Amortization = 
    Amortization {
        amPrincipal :: Currency,
        amApr :: Fixed E6,
        amDuration :: Int,
        amPaymentPeriodicity :: Periodicity,
        amInterestPeriodicity :: Periodicity
    } deriving (Show)

simple :: Amortization
simple = Amortization {
    amPrincipal = Currency 100000.00 "R$",
    amApr = 1.5,
    amDuration = 7,
    amPaymentPeriodicity = Month,
    amInterestPeriodicity = Month
}

-- '-------------------------------------------------------------
-- '
-- '  Name                      : FV
-- '  Purpose                   : It is computed as -

-- '                                                   (1+rate)^nper - 1
-- '       fv = -pv*(1+rate)^nper - PMT*(1+rate*type)* -----------------
-- '                                                         rate
-- '
-- '       fv = -pv - PMT * nper        : if rate == 0
-- '
-- '
-- '  Returns                   : Double
-- '
-- '-------------------------------------------------------------
-- futureValue :: Double -> Double -> Double -> Int -> Int -> Double
-- futureValue pv pmt rate period due
--     | rate == 0 = - pv - pmt * fromIntegral period
--     | otherwise = ((-pv) * dTemp2) - ((pmt / rate) * dTemp * (dTemp2 - 1))
--         where        
--             dTemp = 1 + rate * fromIntegral due
--             dTemp2 = dTemp3 ^ period
--             dTemp3 = 1 + rate     
-- '-------------------------------------------------------------
-- second try 
-- FV = PV(1+ r)^n
futureValue :: Double -> Double -> Int -> Double
futureValue pv r n = pv * ((1 + r)^n)

-- future value of periodic payments
-- payment due at end of periods
-- FV = PV(1 + (r / k)^nk + PMT( (1 + (r / k))^nk - 1 / (r / k))
-- src: https://keisan.casio.com/exec/system/1234231998
-- where 
--      pv is present value
--      pmt is payment amount
--      r is rate
--      n is periods in year
--      k is payment frequency in period (1 / 2 / 4 / 12)
futureValueOfPeriodicPaymentsDueAtEndOfPeriods :: Double -> Double -> Double -> Int -> Int -> Double
futureValueOfPeriodicPaymentsDueAtEndOfPeriods pv pmt r n k = 
    futureValue pv rk nk
    + pmt * ( (((1 + rk) ^ nk) - 1) / rk)
    where
        rk = r / fromIntegral k
        nk = n * k


-- future value of periodic payments
-- payment due at begin of periods
-- FV = PV(1 + (r / k)^nk + PMT( (1 + (r / k))^nk - 1 / (r / k) * 1 + rk)
-- src: https://keisan.casio.com/exec/system/1234231998
-- where 
--      pv is present value
--      pmt is payment amount
--      r is rate
--      n is periods in year
--      k is payment frequency in period (1 / 2 / 4 / 12)
futureValueOfPeriodicPaymentsDueAtBeginOfPeriods :: Double -> Double -> Double -> Int -> Int -> Double
futureValueOfPeriodicPaymentsDueAtBeginOfPeriods pv pmt r n k =  
    futureValue pv rk nk
    + (pmt * ( (((1 + rk) ^ nk) - 1) / rk)) * (1 + rk)
    where
        rk = r / fromIntegral k
        nk = n * k

-- '
-- '  Name                      : PMT
-- '  Purpose                   :
-- '               This function, together with the four following
-- '               it (Pv, Fv, NPer and Rate), can calculate
-- '               a certain value associated with a regular series of
-- '               equal-sized payments.  This series can be fully described
-- '               by these values:
-- '                     Pv   - present value
-- '                     Fv   - future value (at end of series)
-- '                     PMT  - the regular payment
-- '                     nPer - the number of 'periods' over which the
-- '                            money is paid
-- '                     Rate - the interest rate per period.
-- '                            (type - payments at beginning (1) or end (0) of
-- '                            the period).
-- '               Each function can determine one of the values, given the others.
-- '
-- '               General Function for the above values:
-- '
-- '                                                      (1+rate)^nper - 1
-- '               pv * (1+rate)^nper + PMT*(1+rate*type)*----------------- + fv  = 0
-- '                                                            rate
-- '               rate == 0  ->  pv + PMT*nper + fv = 0
-- '
-- '               Thus:
-- '                     (-fv - pv*(1+rate)^nper) * rate
-- '               PMT = -------------------------------------
-- '                     (1+rate*type) * ( (1+rate)^nper - 1 )
-- '
-- '               PMT = (-fv - pv) / nper    : if rate == 0


main :: IO ()
main = do
    print "hello world"
    print(Amortization {
    amPrincipal = Currency 100000.00 "R$",
    amApr = 1.5,
    amDuration = 7,
    amPaymentPeriodicity = Month,
    amInterestPeriodicity = Month
})