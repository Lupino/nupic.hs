{-# LANGUAGE RecordWildCards #-}
module Nupic.Algorithms.TemporalMemory
  ( Options (..)
  , options
  , TemporalMemory
  , new
  , version
  , seed_
  , reset
  , computeRaw
  , compute
  , compute'
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
import           Foreign.Hoppy.Runtime  (fromContents)
import           Foreign.Nupic.Internal (UIntVector,
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
import qualified Foreign.Nupic.Internal as I (TemporalMemory)
import           Nupic.Types.Internal   (getUIntArray)
import           Nupic.Types.Sdr        (Sdr, newSdr, setSparse)

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

data TemporalMemory = TM Options I.TemporalMemory

new :: Options -> IO TemporalMemory
new opt@Options {..} = do
  dims <- fromContents columnDimensions :: IO UIntVector
  tm <- temporalMemory_new
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

  return $ TM opt tm

version :: TemporalMemory -> IO CUInt
version (TM _ tm) = temporalMemory_version tm

seed_ :: TemporalMemory -> CLong -> IO ()
seed_ (TM _ tm) = temporalMemory_seed_ tm

computeRaw :: TemporalMemory -> Sdr -> Bool -> Sdr -> Sdr -> IO ()
computeRaw (TM _ tm) = temporalMemory_compute tm

compute :: TemporalMemory -> Sdr -> Bool -> IO ()
compute tm sdr learn = do
  extraActive <- newSdr [0]
  extraWinners <- newSdr [0]
  computeRaw tm sdr learn extraActive extraWinners

compute' :: TemporalMemory -> [CUInt] -> Bool -> IO ()
compute' tm@(TM opt _) arr learn = do
  sdr <- newSdr $ columnDimensions opt
  setSparse sdr arr
  compute tm sdr learn

getActiveCells :: TemporalMemory -> IO [CUInt]
getActiveCells (TM _ tm) = getUIntArray =<< temporalMemory_getActiveCells tm

getPredictiveCells :: TemporalMemory -> IO [CUInt]
getPredictiveCells (TM _ tm) =
  getUIntArray =<< temporalMemory_getPredictiveCells tm

getWinnerCells :: TemporalMemory -> IO [CUInt]
getWinnerCells (TM _ tm) = getUIntArray =<< temporalMemory_getWinnerCells tm

reset :: TemporalMemory -> IO ()
reset (TM _ tm) = temporalMemory_reset tm

numberOfCells :: TemporalMemory -> IO CUInt
numberOfCells (TM _ tm) = temporalMemory_numberOfCells tm

activateDendrites :: TemporalMemory -> Bool -> [CUInt] -> [CUInt] -> IO ()
activateDendrites (TM _ tm) learn extraActive extraWinners = do
  v0 <- fromContents extraActive :: IO UIntVector
  v1 <- fromContents extraWinners :: IO UIntVector
  temporalMemory_activateDendrites tm learn v0 v1

activateDendrites' :: TemporalMemory -> IO ()
activateDendrites' tm = activateDendrites tm True [maxBound] [maxBound]

saveToFile :: TemporalMemory -> FilePath -> IO ()
saveToFile (TM _ tm) = temporalMemory_saveToFile tm

loadFromFile :: TemporalMemory -> FilePath -> IO ()
loadFromFile (TM _ tm) = temporalMemory_loadFromFile tm
