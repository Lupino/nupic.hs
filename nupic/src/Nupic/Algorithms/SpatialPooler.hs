{-# LANGUAGE RecordWildCards #-}

module Nupic.Algorithms.SpatialPooler
  ( SpatialPooler
  , Options (..)
  , options
  , new
  , compute
  , compute_
  , getNumColumns
  , getIterationNum
  , saveToFile
  , loadFromFile
  , module Exports
  ) where

import           Foreign.C              (CUInt)
import           Foreign.Hoppy.Runtime  (fromContents, toContents)
import           Foreign.Marshal.Array  (newArray, peekArray)
import           Foreign.Nupic.Internal (ByteVector, Real64Vector,
                                         SpatialPooler, UByteVector, UIntVector)
import           Foreign.Nupic.Internal as Exports (spatialPooler_compute,
                                                    spatialPooler_computeWithPtr,
                                                    spatialPooler_getIterationNum,
                                                    spatialPooler_getNumColumns,
                                                    spatialPooler_loadFromFile,
                                                    spatialPooler_new,
                                                    spatialPooler_saveToFile)
import           Nupic.Types.Sdr        (Sdr)

data Options = Options
  { inputDimensions            :: [CUInt]
  , columnDimensions           :: [CUInt]
  , potentialRadius            :: CUInt
  , potentialPct               :: Float
  , globalInhibition           :: Bool
  , localAreaDensity           :: Float
  , numActiveColumnsPerInhArea :: Int
  , stimulusThreshold          :: CUInt
  , synPermInactiveDec         :: Float
  , synPermActiveInc           :: Float
  , synPermConnected           :: Float
  , minPctOverlapDutyCycles    :: Float
  , dutyCyclePeriod            :: CUInt
  , boostStrength              :: Float
  , seed                       :: Int
  , spVerbosity                :: CUInt
  , wrapAround                 :: Bool
  }

options :: [CUInt] -> [CUInt] -> Options
options v0 v1 = Options
  { inputDimensions            = v0
  , columnDimensions           = v1
  , potentialRadius            = 26
  , potentialPct               = 0.05
  , globalInhibition           = True
  , localAreaDensity           = -1
  , numActiveColumnsPerInhArea = 10
  , stimulusThreshold          = 0
  , synPermInactiveDec         = 0.008
  , synPermActiveInc           = 0.05
  , synPermConnected           = 0.1
  , minPctOverlapDutyCycles    = 0.001
  , dutyCyclePeriod            = 1000
  , boostStrength              = 0.0
  , seed                       = 1
  , spVerbosity                = 0
  , wrapAround                 = True
  }

new :: Options -> IO SpatialPooler
new Options {..} = do
  v0 <- fromContents inputDimensions :: IO UIntVector
  v1 <- fromContents columnDimensions :: IO UIntVector
  spatialPooler_new
    v0
    v1
    potentialRadius
    potentialPct
    globalInhibition
    localAreaDensity
    numActiveColumnsPerInhArea
    stimulusThreshold
    synPermInactiveDec
    synPermActiveInc
    synPermConnected
    minPctOverlapDutyCycles
    dutyCyclePeriod
    boostStrength
    seed
    spVerbosity
    wrapAround

compute :: SpatialPooler -> Sdr -> Bool -> Sdr -> IO ()
compute = spatialPooler_compute

compute_ :: SpatialPooler -> [CUInt] -> Bool -> [CUInt] -> IO [CUInt]
compute_ sp input learn active = do
  input' <- newArray input
  active' <- newArray active
  spatialPooler_computeWithPtr sp input' learn active'
  peekArray (length active) active'

getIterationNum :: SpatialPooler -> IO CUInt
getIterationNum = spatialPooler_getIterationNum

getNumColumns :: SpatialPooler -> IO CUInt
getNumColumns = spatialPooler_getNumColumns

saveToFile :: SpatialPooler -> FilePath -> IO ()
saveToFile = spatialPooler_saveToFile

loadFromFile :: SpatialPooler -> FilePath -> IO ()
loadFromFile = spatialPooler_loadFromFile
