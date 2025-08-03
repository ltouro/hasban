{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.GoalSeeker
  ( GoalSeekResult(..)
  , GoalSeekConfig(..)
  , goalSeek
  , defaultGoalSeekConfig
  , GoalSeekError(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | Configuration for goal seek algorithm
data GoalSeekConfig = GoalSeekConfig
  { maxIterations   :: Int     -- ^ Maximum number of iterations
  , tolerance       :: Double  -- ^ Convergence tolerance
  , stepSize        :: Double  -- ^ Initial step size for derivative approximation
  } deriving (Eq, Show, Generic)

instance FromJSON GoalSeekConfig
instance ToJSON GoalSeekConfig

-- | Default configuration for goal seek
defaultGoalSeekConfig :: GoalSeekConfig
defaultGoalSeekConfig = GoalSeekConfig
  { maxIterations = 100
  , tolerance = 1e-6
  , stepSize = 1e-5
  }

-- | Possible errors from goal seek algorithm
data GoalSeekError = 
    MaxIterationsExceeded Int      -- ^ Maximum iterations reached
  | NumericalInstability String    -- ^ Numerical issues encountered
  | InvalidFunction String         -- ^ Function evaluation issues
  deriving (Eq, Show, Generic)

instance FromJSON GoalSeekError
instance ToJSON GoalSeekError

-- | Result of goal seek operation
data GoalSeekResult = GoalSeekResult
  { solution      :: Double    -- ^ Found solution
  , iterations    :: Int       -- ^ Number of iterations used
  , finalError    :: Double    -- ^ Final function value at solution
  , converged     :: Bool      -- ^ Whether algorithm converged
  } deriving (Eq, Show, Generic)

instance FromJSON GoalSeekResult
instance ToJSON GoalSeekResult

-- | Goal seek using Newton-Raphson method with numerical derivatives
-- 
-- Finds x such that f(x) = target, using iterative refinement.
-- This is useful for solving equations like "find loan amount such that net value equals target"
goalSeek :: GoalSeekConfig
         -> (Double -> Double)  -- ^ Function to optimize
         -> Double              -- ^ Target value
         -> Double              -- ^ Initial guess
         -> Either GoalSeekError GoalSeekResult
goalSeek config f target initialGuess = 
  goalSeekLoop config f target initialGuess 0

goalSeekLoop :: GoalSeekConfig -> (Double -> Double) -> Double -> Double -> Int -> Either GoalSeekError GoalSeekResult
goalSeekLoop config f target currentGuess iteration
  | iteration >= maxIterations config = 
      Left (MaxIterationsExceeded iteration)
  | otherwise = 
      let currentValue = f currentGuess
          error' = currentValue - target
          absError = abs error'
      in if absError <= tolerance config
         then Right $ GoalSeekResult
           { solution = currentGuess
           , iterations = iteration
           , finalError = error'
           , converged = True
           }
         else do
           derivative <- calculateDerivative config f currentGuess
           if abs derivative < 1e-15
             then Left (NumericalInstability "Derivative too small (near zero)")
             else let nextGuess = currentGuess - error' / derivative
                  in goalSeekLoop config f target nextGuess (iteration + 1)

-- | Calculate numerical derivative using finite differences
calculateDerivative :: GoalSeekConfig -> (Double -> Double) -> Double -> Either GoalSeekError Double
calculateDerivative config f x = 
  let h = stepSize config
      x1 = x - h
      x2 = x + h
      y1 = f x1
      y2 = f x2
      derivative = (y2 - y1) / (2 * h)
  in if isNaN derivative || isInfinite derivative
     then Left (NumericalInstability "Invalid derivative calculation")
     else Right derivative

{- |
Goal Seek Implementation Notes:

This module implements a robust numerical goal seek algorithm using the Newton-Raphson method
with numerical derivatives. It's designed for solving equations where we need to find an input
value that produces a specific output value.

Common use cases in financial calculations:
1. Finding loan amount that produces a specific net value
2. Finding interest rate that produces a specific payment
3. Finding term length that produces a specific total cost

The algorithm uses:
- Newton-Raphson method for fast convergence
- Numerical derivatives to avoid requiring analytical derivatives
- Configurable tolerance and iteration limits
- Comprehensive error handling for numerical edge cases

Convergence is typically quadratic when the function is well-behaved near the solution.
-}