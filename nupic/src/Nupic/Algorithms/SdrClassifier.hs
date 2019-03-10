module Nupic.Algorithms.SdrClassifier
  ( SdrClassifier
  , new
  , compute
  , saveToFile
  , loadFromFile
  ) where

import           Foreign.C                    (CUInt)
import           Foreign.Hoppy.Runtime        (fromContents, toContents)
import           Foreign.Nupic.Internal       (ByteVector, Real64Vector,
                                               SdrClassifier, UByteVector,
                                               UIntVector)
import           Foreign.Nupic.Internal       as Exports (sdrClassifier_compute, sdrClassifier_loadFromFile,
                                                          sdrClassifier_new,
                                                          sdrClassifier_saveToFile)
import           Nupic.Types.ClassifierResult (ClassifierResult,
                                               newClassifierResult)


new :: [CUInt] -> Double -> Double -> CUInt -> IO SdrClassifier
new steps alpha actValueAlpha verbosity = do
  steps' <- fromContents steps :: IO UIntVector
  sdrClassifier_new steps' alpha actValueAlpha verbosity

compute :: SdrClassifier -> CUInt -> [CUInt] -> [CUInt] -> [Double] -> Bool -> Bool -> Bool -> IO ClassifierResult
compute clsr recordNum patternNZ bucketIdxList actValueList category learn infer = do
  v0 <- fromContents patternNZ :: IO UIntVector
  v1 <- fromContents bucketIdxList :: IO UIntVector
  v2 <- fromContents actValueList :: IO Real64Vector
  result <- newClassifierResult
  sdrClassifier_compute clsr recordNum v0 v1 v2 category learn infer result
  return result

saveToFile :: SdrClassifier -> FilePath -> IO ()
saveToFile = sdrClassifier_saveToFile

loadFromFile :: SdrClassifier -> FilePath -> IO ()
loadFromFile = sdrClassifier_loadFromFile
