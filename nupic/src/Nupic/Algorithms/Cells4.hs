{-# LANGUAGE RecordWildCards #-}
module Nupic.Algorithms.Cells4
  ( new
  , Cells4
  , Options (..)
  , options
  , compute
  , nCells
  , saveToFile
  , loadFromFile
  ) where

import           Foreign.C              (CFloat, CUInt)
import           Foreign.Marshal.Array  (newArray, peekArray)
import           Foreign.Nupic.Internal (Cells4, cells4_compute,
                                         cells4_loadFromFile, cells4_nCells,
                                         cells4_new, cells4_saveToFile)

data Options = Options
  { nColumns                :: CUInt
  , nCellsPerCol            :: CUInt
  , activationThreshold     :: CUInt
  , minThreshold            :: CUInt
  , newSynapseCount         :: CUInt
  , segUpdateValidDuration  :: CUInt
  , permInitial             :: Float
  , permConnected           :: Float
  , permInc                 :: Float
  , permMax                 :: Float
  , permDec                 :: Float
  , globalDecay             :: Float
  , doPooling               :: Bool
  , seed                    :: Int
  , initFromCpp             :: Bool
  , checkSynapseConsistency :: Bool
  }

options :: Options
options = Options
  { nColumns                = 0
  , nCellsPerCol            = 0
  , activationThreshold     = 1
  , minThreshold            = 1
  , newSynapseCount         = 1
  , segUpdateValidDuration  = 1
  , permInitial             = 0.5
  , permConnected           = 0.8
  , permMax                 = 1
  , permDec                 = 0.1
  , permInc                 = 0.1
  , globalDecay             = 0.0
  , doPooling               = False
  , seed                    = -1
  , initFromCpp             = True
  , checkSynapseConsistency = False
  }

new :: Options -> IO Cells4
new Options {..} =
  cells4_new
    nColumns
    nCellsPerCol
    activationThreshold
    minThreshold
    newSynapseCount
    segUpdateValidDuration
    permInitial
    permConnected
    permMax
    permDec
    permInc
    globalDecay
    doPooling
    seed
    initFromCpp
    checkSynapseConsistency

compute :: Cells4 -> [CFloat] -> Bool -> Bool -> IO [CFloat]
compute cells4 input doInference doLearning = do
  inputPtr <- newArray input
  ncells <- fromIntegral <$> nCells cells4
  outputPtr <- newArray $ replicate ncells 0.0
  cells4_compute cells4 inputPtr outputPtr doInference doLearning
  peekArray ncells outputPtr

nCells :: Cells4 -> IO CUInt
nCells = cells4_nCells

saveToFile :: Cells4 -> FilePath -> IO ()
saveToFile = cells4_saveToFile

loadFromFile :: Cells4 -> FilePath -> IO ()
loadFromFile = cells4_loadFromFile
