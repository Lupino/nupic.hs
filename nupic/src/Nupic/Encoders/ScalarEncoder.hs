module Nupic.Encoders.ScalarEncoder
  ( new
  , encode
  , getOutputWidth
  ) where

import           Foreign.C              (CUInt)
import           Foreign.Marshal.Array  (newArray, peekArray)
import           Foreign.Nupic.Internal (ScalarEncoder,
                                         scalarEncoder_encodeIntoArray,
                                         scalarEncoder_getOutputWidth,
                                         scalarEncoder_new)

-- ScalarEncoder(int w, double minValue, double maxValue, int n, double radius,
--               double resolution, bool clipInput);
new :: Int -> Double -> Double -> Int -> Double -> Double -> Bool -> IO ScalarEncoder
new = scalarEncoder_new

getOutputWidth :: ScalarEncoder -> IO CUInt
getOutputWidth = scalarEncoder_getOutputWidth

encode :: ScalarEncoder -> Float -> IO (Int, [CUInt])
encode sc input = do
  len <- fromIntegral <$> getOutputWidth sc
  ptr <- newArray $ replicate len 0
  bucketId <- scalarEncoder_encodeIntoArray sc input ptr
  arr <- peekArray len ptr
  return (bucketId, arr)
