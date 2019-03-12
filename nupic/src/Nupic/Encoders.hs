{-# LANGUAGE RecordWildCards #-}
module Nupic.Encoders
  ( scalarEncoder
  , scalarEncoder_
  , ScalarEncoder
  , encode
  , decodeWithBucket
  ) where

import           Foreign.C (CUInt)

data ScalarEncoder = ScalarEncoder
  { w           :: Int
  , minValue    :: Double
  , maxValue    :: Double
  , n           :: Int
  , bucketWidth :: Double
  , clipInput   :: Bool
  }

scalarEncoder :: Int -> Double -> Double -> Int -> Bool -> ScalarEncoder
scalarEncoder w minValue maxValue n clipInput =
  scalarEncoder_ w minValue maxValue n 0.0 0.0 clipInput

scalarEncoder_ :: Int -> Double -> Double -> Int -> Double -> Double -> Bool -> ScalarEncoder
scalarEncoder_ w minValue maxValue n radius resolution clipInput
  | w <= 0           = error "w must be > 0"
  | extentWidth <= 0 = error $ "minValue max be < maxValue. minValue=" ++ show minValue ++ " maxValue=" ++ show maxValue
  | n > 0            = ScalarEncoder {bucketWidth = bucketWidth0, ..}
  | bucketWidth1 <= 0 = error "One of n/radius/resolution must be nonzero."
  | otherwise = ScalarEncoder {n = n0, bucketWidth = bucketWidth1, ..}
  where extentWidth = maxValue - minValue
        nBuckets = n - (w - 1)
        nBands = nBuckets - 1
        bucketWidth0 = extentWidth / fromIntegral nBands
        bucketWidth1 = if resolution > 0 then resolution else radius / fromIntegral w
        neededBands = ceiling (extentWidth / bucketWidth1) :: Int
        neededBuckets = neededBands + 1
        n0 = neededBuckets + (w - 1)

encode :: ScalarEncoder -> Double -> (Int, [CUInt])
encode enc@ScalarEncoder {..} input0
  | clipInput = encode_ enc input1
  | otherwise = encode_ enc input0
  where input1 = if input0 < minValue then minValue else if input0 > maxValue then maxValue else input0

encode_ :: ScalarEncoder -> Double -> (Int, [CUInt])
encode_ ScalarEncoder {..} input
  | input < minValue || input > maxValue = error "Input must be within [minValue, maxValue]"
  | otherwise = (iBucket, take n $ h ++ s ++ t)
  where iBucket = round((input - minValue) / bucketWidth)
        h = replicate iBucket 0
        s = replicate w 1
        t = replicate n 0

decodeWithBucket :: ScalarEncoder -> Int -> Double
decodeWithBucket ScalarEncoder {..} iBucket = fromIntegral iBucket * bucketWidth + minValue
