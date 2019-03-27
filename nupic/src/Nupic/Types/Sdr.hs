module Nupic.Types.Sdr
  ( newSdr
  , Sdr
  , getDimensions
  , getDense
  , setDense
  , setDense'
  , setSparse
  , getSparse
  , binaryToSparse
  , sparseToBinary
  ) where

import           Foreign.C              (CChar, CUChar, CUInt)
import           Foreign.Hoppy.Runtime  (fromContents)
import           Foreign.Nupic.Internal (ByteVector, Sdr, UByteVector,
                                         UIntVector, sdr_dimensions,
                                         sdr_getDense, sdr_getSparse, sdr_new,
                                         sdr_setDense, sdr_setDenseWithUChar,
                                         sdr_setSparse)
import           Nupic.Types.Internal   (getCharArray, getUIntArray)

newSdr :: [CUInt] -> IO Sdr
newSdr dimensions = do
  v <- fromContents dimensions  :: IO UIntVector
  sdr_new v

getDimensions :: Sdr -> IO [CUInt]
getDimensions sdr = getUIntArray =<< sdr_dimensions sdr

getDense :: Sdr -> IO [CChar]
getDense sdr = getCharArray =<< sdr_getDense sdr

setDense :: Sdr -> [CChar] -> IO ()
setDense sdr dense = do
  d <- fromContents dense :: IO ByteVector
  sdr_setDense sdr d

setDense' :: Sdr -> [CUChar] -> IO ()
setDense' sdr dense = do
  d <- fromContents dense :: IO UByteVector
  sdr_setDenseWithUChar sdr d

getSparse :: Sdr -> IO [CUInt]
getSparse sdr = getUIntArray =<< sdr_getSparse sdr

setSparse :: Sdr -> [CUInt] -> IO ()
setSparse sdr sparse = do
  s <- fromContents sparse :: IO UIntVector
  sdr_setSparse sdr s

binaryToSparse :: [CChar] -> IO [CUInt]
binaryToSparse c = do
  sdr <- newSdr [fromIntegral $ length c]
  setDense sdr c
  getSparse sdr

sparseToBinary :: CUInt -> [CUInt] -> IO [CChar]
sparseToBinary cols s = do
  sdr <- newSdr [cols]
  setSparse sdr s
  getDense sdr
