module Nupic.Types.Sdr
  ( newSdr
  , Sdr
  , getDimensions
  , getDense
  , setDense
  , setDense'
  , setSparse
  , getSparse
  , module Exports
  ) where

import           Foreign.C              (CChar, CUChar, CUInt)
import           Foreign.Hoppy.Runtime  (fromContents, toContents)
import           Foreign.Nupic.Internal (ByteVector, Sdr, UByteVector,
                                         UIntVector)
import           Foreign.Nupic.Internal as Exports (sdr_dimensions,
                                                    sdr_getDense, sdr_getSparse,
                                                    sdr_new, sdr_setDense,
                                                    sdr_setDenseWithUChar,
                                                    sdr_setSparse)

newSdr :: [CUInt] -> IO Sdr
newSdr dimensions = do
  v <- fromContents dimensions  :: IO UIntVector
  sdr_new v

getDimensions :: Sdr -> IO [CUInt]
getDimensions sdr = do
  dims <- sdr_dimensions sdr
  toContents dims

getDense :: Sdr -> IO [CChar]
getDense sdr = do
  dense <- sdr_getDense sdr
  toContents dense

setDense :: Sdr -> [CChar] -> IO ()
setDense sdr dense = do
  d <- fromContents dense :: IO ByteVector
  sdr_setDense sdr d

setDense' :: Sdr -> [CUChar] -> IO ()
setDense' sdr dense = do
  d <- fromContents dense :: IO UByteVector
  sdr_setDenseWithUChar sdr d

getSparse :: Sdr -> IO [CUInt]
getSparse sdr = do
  sparse <- sdr_getSparse sdr
  toContents sparse

setSparse :: Sdr -> [CUInt] -> IO ()
setSparse sdr sparse = do
  s <- fromContents sparse :: IO UIntVector
  sdr_setSparse sdr s

