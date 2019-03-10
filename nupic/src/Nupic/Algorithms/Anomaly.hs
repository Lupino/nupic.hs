module Nupic.Algorithms.Anomaly
  ( Mode (..)
  , new
  , new'
  , compute
  , compute_
  ) where

import           Foreign.C              (CUInt)
import           Foreign.Hoppy.Runtime  (fromContents)
import           Foreign.Nupic.Internal (Anomaly, AnomalyMode (..), UIntVector,
                                         anomaly_compute, anomaly_new)

data Mode = Pure | Likelihood | Weighted

toAnomalyMode :: Mode -> AnomalyMode
toAnomalyMode Pure       = AnomalyMode_Pure
toAnomalyMode Likelihood = AnomalyMode_Likelihood
toAnomalyMode Weighted   = AnomalyMode_Weighted

--  82   Anomaly(UInt slidingWindowSize = 0, AnomalyMode mode = AnomalyMode::PURE,
--  83           Real32 binaryAnomalyThreshold = 0);
new :: CUInt -> Mode -> Float -> IO Anomaly
new slidingWindowSize mode = anomaly_new slidingWindowSize (toAnomalyMode mode)

new' :: IO Anomaly
new' = new 0 Pure 0.0

compute_ :: Anomaly -> [CUInt] -> [CUInt] -> Int -> IO Float
compute_ an active predicted timestamp = do
  v0 <- fromContents active :: IO UIntVector
  v1 <- fromContents predicted :: IO UIntVector
  anomaly_compute an v0 v1 timestamp

compute :: Anomaly -> [CUInt] -> [CUInt] -> IO Float
compute an active predicted = compute_ an active predicted (-1)
