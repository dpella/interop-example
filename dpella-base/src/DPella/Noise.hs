module DPella.Noise where

import System.Random
import Data.IORef

-- | A type to represent a random number generator, which is an IORef
-- containing a standard generator.
type NoiseGen = IORef StdGen


-- | Create a new random number generator
newNoiseGen :: IO NoiseGen
newNoiseGen =  do
    gen <- newStdGen
    newIORef gen

-- | draw a single sample from a Gaussian distribution with mean mu and standard deviation sigma
dpellaSampleRandom :: NoiseGen -> Double -> Double -> IO Double
dpellaSampleRandom ref mu sigma = do
  gen0 <- readIORef ref
  let (u1, gen1) = randomR (1e-10, 1   ) gen0  -- avoid log 0
      (u2, gen2) = randomR (0      , 1   ) gen1
      z0         = sqrt (-2 * log u1) * cos (2 * pi * u2)
      value      = mu + sigma * z0
  writeIORef ref gen2
  pure value
