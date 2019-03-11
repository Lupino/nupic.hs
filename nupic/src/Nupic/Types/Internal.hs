module Nupic.Types.Internal
  ( getUIntArray
  , getCharArray
  ) where

import           Foreign.C              (CChar, CUInt)
import           Foreign.Hoppy.Runtime  (toContents)
import           Foreign.Nupic.Internal (ByteVector, UIntVector,
                                         byteVector_size, uIntVector_size)

getUIntArray :: UIntVector -> IO [CUInt]
getUIntArray v = do
  size <- uIntVector_size v
  if size > 0 then toContents v
              else return []

getCharArray :: ByteVector -> IO [CChar]
getCharArray v = do
  size <- byteVector_size v
  if size > 0 then toContents v
              else return []
