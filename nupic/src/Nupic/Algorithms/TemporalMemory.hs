{-# LANGUAGE RecordWildCards #-}
module Nupic.Algorithms.TemporalMemory
  ( Options (..)
  , options
  , TemporalMemory
  , new
  , version
  , seed_
  , reset
  , compute_
  , compute_'
  , compute
  , getWinnerCells
  , getPredictiveCells
  , getActiveCells
  , activateDendrites
  , activateDendrites'
  , saveToFile
  , loadFromFile
  , numberOfCells
  ) where

import           Foreign.C              (CLong, CUInt)
import           Foreign.Hoppy.Runtime  (fromContents, toContents)
import           Foreign.Nupic.Internal (TemporalMemory, UIntVector,
                                         temporalMemory_activateDendrites,
                                         temporalMemory_compute,
                                         temporalMemory_getActiveCells,
                                         temporalMemory_getPredictiveCells,
                                         temporalMemory_getWinnerCells,
                                         temporalMemory_loadFromFile,
                                         temporalMemory_new,
                                         temporalMemory_numberOfCells,
                                         temporalMemory_reset,
                                         temporalMemory_saveToFile,
                                         temporalMemory_seed_,
                                         temporalMemory_version)
import           Nupic.Types.Sdr        (Sdr, getDense, newSdr, setDense)

data Options = Options
  { columnDimensions          :: [CUInt]
  , cellsPerColumn            :: CUInt
  , activationThreshold       :: CUInt
  , initialPermanence         :: Float
  , connectedPermanence       :: Float
  , minThreshold              :: CUInt
  , maxNewSynapseCount        :: CUInt
  , permanenceIncrement       :: Float
  , permanenceDecrement       :: Float
  , predictedSegmentDecrement :: Float
  , seed                      :: Int
  , maxSegmentsPerCell        :: CUInt
  , maxSynapsesPerSegment     :: CUInt
  , checkInputs               :: Bool
  , extra                     :: CUInt
  }

options :: [CUInt] -> Options
options dims = Options
  { columnDimensions          = dims
  , cellsPerColumn            = 32
  , activationThreshold       = 13
  , initialPermanence         = 0.21
  , connectedPermanence       = 0.50
  , minThreshold              = 10
  , maxNewSynapseCount        = 20
  , permanenceIncrement       = 0.10
  , permanenceDecrement       = 0.10
  , predictedSegmentDecrement = 0.0
  , seed                      = 42
  , maxSegmentsPerCell        = 255
  , maxSynapsesPerSegment     = 255
  , checkInputs               = True
  , extra                     = 0
  }

new :: Options -> IO TemporalMemory
new Options {..} = do
  dims <- fromContents columnDimensions :: IO UIntVector
  temporalMemory_new
    dims
    cellsPerColumn
    activationThreshold
    initialPermanence
    connectedPermanence
    minThreshold
    maxNewSynapseCount
    permanenceIncrement
    permanenceDecrement
    predictedSegmentDecrement
    seed
    maxSegmentsPerCell
    maxSynapsesPerSegment
    checkInputs
    extra

version :: TemporalMemory -> IO CUInt
version = temporalMemory_version

seed_ :: TemporalMemory -> CLong -> IO ()
seed_ = temporalMemory_seed_

compute_ :: TemporalMemory -> Sdr -> Bool -> Sdr -> Sdr -> IO ()
compute_ = temporalMemory_compute

compute_' :: TemporalMemory -> Sdr -> Bool -> IO ()
compute_' tm sdr learn = do
  extraActive <- newSdr [0]
  extraWinners <- newSdr [0]
  compute_ tm sdr learn extraActive extraWinners

compute :: TemporalMemory -> [CUInt] -> Bool -> IO ()
compute tm arr learn = do
  sdr <- newSdr [fromIntegral $ length arr]
  setDense sdr $ map fromIntegral arr
  compute_' tm sdr learn

getActiveCells :: TemporalMemory -> IO [CUInt]
getActiveCells tm = toContents =<< temporalMemory_getActiveCells tm

getPredictiveCells :: TemporalMemory -> IO [CUInt]
getPredictiveCells tm = toContents =<< temporalMemory_getPredictiveCells tm

getWinnerCells :: TemporalMemory -> IO [CUInt]
getWinnerCells tm = toContents =<< temporalMemory_getWinnerCells tm

reset :: TemporalMemory -> IO ()
reset = temporalMemory_reset

numberOfCells :: TemporalMemory -> IO CUInt
numberOfCells = temporalMemory_numberOfCells

activateDendrites :: TemporalMemory -> Bool -> [CUInt] -> [CUInt] -> IO ()
activateDendrites tm learn extraActive extraWinners = do
  v0 <- fromContents extraActive :: IO UIntVector
  v1 <- fromContents extraWinners :: IO UIntVector
  temporalMemory_activateDendrites tm learn v0 v1

activateDendrites' :: TemporalMemory -> IO ()
activateDendrites' tm = activateDendrites tm True [maxBound] [maxBound]

saveToFile :: TemporalMemory -> FilePath -> IO ()
saveToFile = temporalMemory_saveToFile

loadFromFile :: TemporalMemory -> FilePath -> IO ()
loadFromFile = temporalMemory_loadFromFile
