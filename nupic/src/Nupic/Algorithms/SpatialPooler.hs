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
  ) where

import           Foreign.C              (CUInt)
import           Foreign.Hoppy.Runtime  (fromContents)
import           Foreign.Nupic.Internal (UIntVector, spatialPooler_compute,
                                         spatialPooler_computeWithPtr,
                                         spatialPooler_getIterationNum,
                                         spatialPooler_getNumColumns,
                                         spatialPooler_loadFromFile,
                                         spatialPooler_new,
                                         spatialPooler_saveToFile)
import qualified Foreign.Nupic.Internal as I (SpatialPooler)
import           Nupic.Types.Sdr        (Sdr, getDense, newSdr, setDense)

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

data SpatialPooler = SpatialPooler Options I.SpatialPooler

new :: Options -> IO SpatialPooler
new opt@Options {..} = do
  v0 <- fromContents inputDimensions :: IO UIntVector
  v1 <- fromContents columnDimensions :: IO UIntVector
  sp <- spatialPooler_new
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

  return $ SpatialPooler opt sp

compute_ :: SpatialPooler -> Sdr -> Bool -> IO Sdr
compute_ (SpatialPooler opt sp) sdr learn = do
  sdrO <- newSdr $ columnDimensions opt
  spatialPooler_compute sp sdr learn sdrO
  return sdrO

compute :: SpatialPooler -> [CUInt] -> Bool -> IO [CUInt]
compute sp@(SpatialPooler opt _) input learn = do
  sdrI <- newSdr $ inputDimensions opt
  setDense sdrI $ map fromIntegral input
  sdrO <- compute_ sp sdrI learn
  map fromIntegral <$> getDense sdrO

getIterationNum :: SpatialPooler -> IO CUInt
getIterationNum (SpatialPooler _ sp) = spatialPooler_getIterationNum sp

getNumColumns :: SpatialPooler -> IO CUInt
getNumColumns (SpatialPooler _ sp)= spatialPooler_getNumColumns sp

saveToFile :: SpatialPooler -> FilePath -> IO ()
saveToFile (SpatialPooler _ sp)= spatialPooler_saveToFile sp

loadFromFile :: SpatialPooler -> FilePath -> IO ()
loadFromFile (SpatialPooler _ sp) = spatialPooler_loadFromFile sp
