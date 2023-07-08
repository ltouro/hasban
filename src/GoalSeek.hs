module GoalSeek
  ( goalSeek
  , AccuracyLevel
  , Guess
  , MaxIterations
  ) where

type AccuracyLevel = Double
type Guess = Double
type MaxIterations = Int

goalSeek
  :: (Double -> Double)  -- ^ The function to optimize.
  -> AccuracyLevel       -- ^ The desired accuracy level.
  -> Guess               -- ^ The initial guess.
  -> MaxIterations       -- ^ The maximum number of iterations.
  -> Maybe Double              -- ^ The optimized value.
goalSeek function accuracyLevel guess maxIterations =
  let stepSize = 0.00001
      newGuess = guess + stepSize
      result1 = function guess
      result2 = function newGuess
      nextGuess = guess - (result1 * (newGuess - guess) / (result2 - result1))
  in  case (abs result1 <= accuracyLevel, maxIterations) of
        (True, _) -> Just guess
        (_, 0) -> Nothing
        _ -> goalSeek function accuracyLevel nextGuess (maxIterations - 1)
