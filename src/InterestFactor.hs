{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module InterestFactor (
  interestUnitValue
  , interestFactor252BDays
  , interestFactor252Months21
  , interestFactor252MonthsIrregular
  , interestRate36XDays
  , interestRate36XMonths
  , interestRate36XMonthsIrregular
  , DaysInYear(..)
) where

type InterestRate = Double
type CurrencyAmount = Double
type BusinessDays = Integer
type Months = Integer
type Days = Integer
data DaysInYear = Days360 | Days365
  deriving (Show, Eq)

fromDaysInYear :: DaysInYear -> Days
fromDaysInYear d = case d of
    Days360 -> 360
    Days365 -> 365

-- Valor unitário dos juros acumulados no período, calculado com 8 (oito) casas decimais
interestUnitValue :: CurrencyAmount -> InterestRate -> CurrencyAmount
interestUnitValue nominalValue interestFactor = nominalValue * (interestFactor - 1)

-- 252 – número de dias úteis entre a data de início ou do último pagamento e o próximo
interestFactor252BDays
  :: InterestRate -- Taxa de juros expressa em 252 dias úteis, informada com 4 (quatro) casas decimais;
  -> BusinessDays -- Número de dias úteis entre a data de emissão e o evento específico (dut)
  -> BusinessDays -- Número de dias úteis entre a data de emissão e a data de atualização (dup)
  -> InterestRate
interestFactor252BDays interestRate dut dup = let
      interestRatePlusOne = (interestRate / 100) + 1
      factor1 = fromIntegral dut / 252
      factor2 = fromIntegral dup / fromIntegral dut
    in (interestRatePlusOne ** factor1) ** factor2

-- “252 – Número de meses entre a data de início ou último pagamento e o próximo x 21” (data de emissão casada)
interestFactor252Months21
  :: InterestRate   -- Taxa de juros expressa em 252 dias úteis
  -> Months         -- Número de meses entre a data de emissão e o evento específico
  -> BusinessDays   -- Número de dias úteis entre a data de emissão e a data de atualização (dup)
  -> InterestRate
interestFactor252Months21 interestRate months dup = let
      interestRatePlusOne = (interestRate / 100) + 1
      dut = months * 21
      factor1 = fromIntegral dut / 252
      factor2 = fromIntegral dup / fromIntegral dut
    in (interestRatePlusOne ** factor1) ** factor2

 -- “252 – Número de meses entre a data de início ou último pagamento e o próximo x 21”(data de emissão descasada)
interestFactor252MonthsIrregular
  :: InterestRate   -- Taxa de juros expressa em 252 dias úteis
  -> BusinessDays   -- Número de dias úteis entre a data de emissão e a data de atualização ou aniversário mensal imediatamente posterior (duppro-rata)
  -> BusinessDays   -- Número de dias úteis entre a data de aniversário mensal imediatamente anterior à data de emissão e a data de aniversário mensal imediatamente posterior à data de emissão (dutpro-rata)
  -> Months         -- Número de meses entre a data de aniversário imediatamente posterior à data de emissão e a data de vencimento ou 1º evento de juros (Nº meses)
  -> BusinessDays   -- Número de dias úteis entre o 1º aniversário mensal imediatamente posterior à data de emissão e a data de atualização, de incorporação inicial, se houver, ou do 1º evento de pagamento de juros caso não haja incorporação (dup)
  -> BusinessDays   -- Número de dias úteis entre o 1º aniversário mensal imediatamente posterior à data de emissão e a data de vencimento ou 1º evento de juros posterior ao 1º aniversário mensal, caso não haja incorporação (dut)
  -> InterestRate
interestFactor252MonthsIrregular interestRate dupProRata dutProRata nMonths dup dut = let
      r1 = (interestRate / 100) + 1
      factor1 = 21 / 252
      factor2 = fromIntegral  dupProRata / fromIntegral dutProRata
      factor3 = fromIntegral nMonths * 21 / 252
      factor4 = fromIntegral dup / fromIntegral dut
    in ((r1 ** factor1) ** factor2) * ((r1 ** factor3) ** factor4)

-- “360 – Número de dias corridos entre a data de início ou último pagamento e o próximo”,
-- “365 – Número de dias corridos entre a data de início ou último pagamento e o próximo”
interestRate36XDays :: InterestRate
  -> Days       -- Número de dias corridos entre entre o último evento de juros e o próximo evento de juros (dct)
  -> Days       -- Número de dias úteis entre o último evento de juros e a data de atualização, próximo evento ou data de vencimento (dcp)
  -> DaysInYear -- Número de dias no ano
  -> InterestRate
interestRate36XDays interestRate dct dcp daysInYear = let
      r1 = (interestRate / 100) + 1 :: Double
      factor1 = fromIntegral dct / fromIntegral (fromDaysInYear daysInYear) :: Double
      factor2 = fromIntegral  dct / fromIntegral dcp :: Double
    in r1 ** factor1

-- “360 – Número de meses entre a data de início ou último pagamento e o próximo x 30”,
-- ou “365 – Número de meses entre a data de início ou último pagamento e o próximo x 30” (data de emissão casada)
interestRate36XMonths 
  :: InterestRate -- Taxa de juros expressa em 360 ou 365 dias corridos, com 4 (quatro) casas decimais
  -> Months       -- Número de meses entre a o último evento de juros e o próximo evento de juros
  -> Days         -- Dias úteis pro-rata - Número de dias corridos entre entre o último evento de juros e o próximo evento de juros (dct)
  -> Days         -- Dias úteis totais - Número de dias úteis entre o último evento de juros e a data de atualização, próximo evento ou data de vencimento (dcp)
  -> DaysInYear   -- Número de dias no ano
  -> InterestRate
interestRate36XMonths r months dcp dct daysInYear = let
      r1 = (r / 100) + 1
      factor1 = (fromIntegral months * 30) / fromIntegral (fromDaysInYear daysInYear)
      factor2 = fromIntegral  dcp / fromIntegral dct
    in (r1 ** factor1) ** factor2

interestRate36XMonthsIrregular
  :: InterestRate -- Taxa de juros expressa em 360 ou 365 dias corridos, com 4 (quatro) casas decimais
  -> Days         -- Número de dias corridos entre a data de emissão e a data de atualização ou aniversário mensal imediatamente posterior(dcp pro-rata)
  -> Days         -- Número de dias corridos entre a data de aniversário mensal imediatamente anterior à data de emissão e a data de aniversário mensal imediatamente posterior à data de emissão(dct pro-rata)
  -> Months       -- Número de meses entre a o último evento de juros e o próximo evento de juros
  -> Days         -- Número de dias corridos entre entre o último evento de juros e o próximo evento de juros (dct)
  -> Days         -- Número de dias úteis entreou entre o último evento de juros e a data de atualização, próximo evento ou data de vencimento (dcp)
  -> DaysInYear   -- Número de dias no ano
  -> InterestRate
interestRate36XMonthsIrregular r dcpProRata dctProRata months dcp dct daysInYear = let
      r1 = (r / 100) + 1
      factor1 = 30 / fromIntegral (fromDaysInYear daysInYear)
      factor2 = fromIntegral  dcpProRata / fromIntegral dctProRata
      factor3 = (fromIntegral months * 30) / fromIntegral (fromDaysInYear daysInYear)
      factor4 = fromIntegral dcp / fromIntegral dct
    in ((r1 ** factor1) ** factor2) * ((r1 ** factor3) ** factor4)
