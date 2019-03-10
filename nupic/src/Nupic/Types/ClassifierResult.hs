module Nupic.Types.ClassifierResult
  ( newClassifierResult
  , ClassifierResult
  , getClass
  ) where

import           Foreign.C              (CUInt)
import           Foreign.Nupic.Internal (ClassifierResult,
                                         classifierResult_getClass,
                                         classifierResult_new)

newClassifierResult :: IO ClassifierResult
newClassifierResult = classifierResult_new

getClass :: ClassifierResult -> CUInt -> IO CUInt
getClass = classifierResult_getClass
