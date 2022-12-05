module Problem1 where 

compareScore :: (Int, Int) -> (Int, Int)
compareScore (a, b)
    | a > b = (1, 0)
    | a < b = (0, 1)
    | otherwise = (0, 0)

compare' :: Int ->Int -> Int
compare' a b
    | a > b = 1    
    | otherwise = 0

data Score = Scor Int Int

compareScore' :: Score -> Score -> Score
compareScore' (Scor a1 a2) (Scor b1 b2) = Scor (compare' a1 b1) (compare' a2 b2)
    
zipTuple :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int)]
zipTuple (a1, a2, a3) (b1, b2, b3) = [(a1, b1), (a2,b2), (a3,b3)]

-- TODO: define the expectedMinutesInOven constant
expectedMinutesInOven :: Int
expectedMinutesInOven = 40

-- TODO: define the preparationTimeInMinutes function
preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes layers = layers * 2

-- TODO: define the elapsedTimeInMinutes function
elapsedTimeInMinutes:: Int -> Int -> Int
elapsedTimeInMinutes layers inOven = preparationTimeInMinutes layers + inOven

tempToC :: Integer -> Float
-- T(°C) = (T(°F) - 32) / 1.8
tempToC fahr = fromIntegral (fahr - 32) / 1.8

--  T(°F) = T(°C) × 1.8 + 32.
tempToF :: Float -> Integer
tempToF cel = ceiling $ (cel * 1.8) + 32

isLeapYear :: Int -> Bool
isLeapYear year 
    | mod year 400 == 0 = True
    | mod year 100 == 0 = False
    | mod year 4 == 0   = True
    | otherwise         = False

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Earth seconds = seconds / 31557600
ageOn Mercury seconds = ageOn Earth (seconds * 0.2408467)
ageOn Venus seconds = ageOn Earth (seconds * 0.61519726)
ageOn Mars seconds = ageOn Earth (seconds * 1.8808158)
ageOn Jupiter seconds= ageOn Earth (seconds * 11.862615)
ageOn Saturn seconds= ageOn Earth (seconds * 29.447498)
ageOn Uranus seconds = ageOn Earth (seconds * 84.016846)
ageOn Neptune seconds= ageOn Earth (seconds * 164.79132)

nub l                   = nub' l []             -- '
  where
    nub' [] _           = []                    -- '
    nub' (x:xs) ls                              -- '
        | x `elem` ls   = nub' xs ls            -- '
        | otherwise     = x : nub' xs (x:ls)    -- '

-- toScore :: [(Int, Int)] -> (Int, Int)
-- toScore as = map scores as
--     where
--         scores = (sum xs, sum ys)
--         xs = fmap (\(a, b) -> a) as
--         ys = fmap (\(a, b) -> b) as

-- compareTriplets :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int)
-- compareTriplets as bs = sum compareA
--     where        
--         tuplesA = zipTuple as bs
--         tuplesB = zipTuple as bs
--         compareA = map compareScore tuplesA

-- $> doSom
-- $> a = 2
-- $> b = 3


-- solveMeFirst a b = a + b

-- main = do
--     val1 <- readLn
--     val2 <- readLn
--     let sum = solveMeFirst val1 val2
--     print sum