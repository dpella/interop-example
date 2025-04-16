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

-- | Sample the random number generator
dpellaSampleRandom :: NoiseGen -> Double -> Double -> IO Double
dpellaSampleRandom ref f t = do
  gen <- readIORef ref
  let (value, newGen) = randomR (f, t) gen
  writeIORef ref newGen
  return value
