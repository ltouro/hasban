module Main where
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
-- '                     (-fv - pv*(1+rate)^nper) * rate
-- '               PMT = -------------------------------------
-- '                        ( (1+rate)^nper - 1 )
paymentForFutureValue :: Double -> Double -> Double -> Integer -> Double
paymentForFutureValue fv pv r n = (-fv - pv * ci) * r / (ci - 1)
    where 
        ci = (1 + r) ^ n

-- $> 
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