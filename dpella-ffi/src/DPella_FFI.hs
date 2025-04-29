{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      DPella.FFI
-- Copyright:   (c) DPella AB 2025
-- License:     LicenseRef-AllRightsReserved
-- Maintainer:  <matti@dpella.io>
-- A module to expose DPella's differential privacy functions to C
module DPella_FFI where

import Foreign.C.Types
import System.IO.Unsafe

import DPella.Noise

-- | Global noise generator, maintains state across wrapped calls
{-# NOINLINE nOISEGEN #-}
nOISEGEN :: NoiseGen
nOISEGEN = unsafePerformIO newNoiseGen

-- | Wrap a function that takes two argument to use the global noise generator
wrap2 :: (NoiseGen -> Double -> Double -> IO Double) -> CDouble -> CDouble -> IO CDouble
wrap2 f x y = do
  res <- f nOISEGEN (realToFrac x) (realToFrac y)
  return (realToFrac res)


-- | Wrapped dpellaSampleRandom function
wrappedDpellaSampleRandom :: CDouble -> CDouble -> IO CDouble
wrappedDpellaSampleRandom = wrap2 dpellaSampleRandom


foreign export ccall "dpella_sample_random_hs"
  wrappedDpellaSampleRandom :: CDouble -> CDouble -> IO CDouble

