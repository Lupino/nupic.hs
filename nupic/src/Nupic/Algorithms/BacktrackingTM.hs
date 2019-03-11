{-# LANGUAGE RecordWildCards #-}
module Nupic.Algorithms.BacktrackingTM
  ( Options (..)
  , options
  , BacktrackingTM
  , new
  , version
  , reset
  , compute
  , infer
  , learn
  , finishLearning
  , predict
  , saveToFile
  , loadFromFile
  ) where

import           Foreign.C              (CFloat, CUInt)
import           Foreign.Marshal.Array  (newArray, peekArray)
import           Foreign.Nupic.Internal (BacktrackingTMCpp, backTM_predict,
                                         backtrackingTMCpp_compute,
                                         backtrackingTMCpp_finishLearning,
                                         backtrackingTMCpp_loadFromFile,
                                         backtrackingTMCpp_new,
                                         backtrackingTMCpp_reset,
                                         backtrackingTMCpp_saveToFile,
                                         backtrackingTMCpp_version)

data Options = Options
  { numberOfCols            :: CUInt
  , cellsPerColumn          :: CUInt
  , initialPerm             :: Float
  , connectedPerm           :: Float
  , minThreshold            :: CUInt
  , newSynapseCount         :: CUInt
  , permanenceInc           :: Float
  , permanenceDec           :: Float
  , permanenceMax           :: Float
  , globalDecay             :: Float
  , activationThreshold     :: CUInt
  , doPooling               :: Bool
  , segUpdateValidDuration  :: CUInt
  , burnIn                  :: CUInt
  , collectStats            :: Bool
  , seed                    :: Int
  , verbosity               :: Int
  , checkSynapseConsistency :: Bool
  , pamLength               :: CUInt
  , maxInfBacktrack         :: CUInt
  , maxLrnBacktrack         :: CUInt
  , maxAge                  :: CUInt
  , maxSeqLength            :: CUInt
  , maxSegmentsPerCell      :: Int
  , maxSynapsesPerSegment   :: Int
  , outputType              :: String
  }

options :: CUInt -> CUInt -> Options
options cols cells = Options
  { numberOfCols            = cols
  , cellsPerColumn          = cells
  , initialPerm             = 0.11
  , connectedPerm           = 0.50
  , minThreshold            = 8
  , newSynapseCount         = 15
  , permanenceInc           = 0.10
  , permanenceDec           = 0.10
  , permanenceMax           = 1.0
  , globalDecay             = 0.10
  , activationThreshold     = 12
  , doPooling               = False
  , segUpdateValidDuration  = 5
  , burnIn                  = 2
  , collectStats            = False
  , seed                    = 42
  , verbosity               = 0
  , checkSynapseConsistency = False
  , pamLength               = 1
  , maxInfBacktrack         = 10
  , maxLrnBacktrack         = 5
  , maxAge                  = 100000
  , maxSeqLength            = 32
  , maxSegmentsPerCell      = -1
  , maxSynapsesPerSegment   = -1
  , outputType              = "normal"
  }

data BacktrackingTM = BacktrackingTM Options BacktrackingTMCpp

new :: Options -> IO BacktrackingTM
new opt@Options {..} = do
  tm <- backtrackingTMCpp_new
    numberOfCols
    cellsPerColumn
    initialPerm
    connectedPerm
    minThreshold
    newSynapseCount
    permanenceInc
    permanenceDec
    permanenceMax
    globalDecay
    activationThreshold
    doPooling
    segUpdateValidDuration
    burnIn
    collectStats
    seed
    verbosity
    checkSynapseConsistency
    pamLength
    maxInfBacktrack
    maxLrnBacktrack
    maxAge
    maxSeqLength
    maxSegmentsPerCell
    maxSynapsesPerSegment
    outputType

  return $ BacktrackingTM opt tm

version :: BacktrackingTM -> IO CUInt
version (BacktrackingTM _ tm)= backtrackingTMCpp_version tm

compute :: BacktrackingTM -> [CFloat] -> Bool -> Bool -> IO [CFloat]
compute (BacktrackingTM (Options {..}) tm) bottomUpInput enableLearn enableInference = do
  input <- newArray bottomUpInput
  r <- backtrackingTMCpp_compute tm input enableLearn enableInference
  peekArray (fromIntegral $ numberOfCols * cellsPerColumn) r

infer :: BacktrackingTM -> [CFloat] -> IO [CFloat]
infer tm input = compute tm input False True

learn :: BacktrackingTM -> [CFloat] -> Bool -> IO [CFloat]
learn tm input = compute tm input True

finishLearning :: BacktrackingTM -> IO ()
finishLearning (BacktrackingTM _ tm) = backtrackingTMCpp_finishLearning tm

reset :: BacktrackingTM -> IO ()
reset (BacktrackingTM _ tm) = backtrackingTMCpp_reset tm

predict :: BacktrackingTM -> CUInt -> IO [[CFloat]]
predict (BacktrackingTM Options{..} tm) nStep = do
  pred <- backTM_predict tm nStep
  mk <$> peekArray (fromIntegral $ nStep * numberOfCols) pred
  where mk :: [CFloat] -> [[CFloat]]
        mk [] = []
        mk xs = take numcols xs: mk (drop numcols xs)
        numcols = fromIntegral numberOfCols

saveToFile :: BacktrackingTM -> FilePath -> IO ()
saveToFile (BacktrackingTM _ tm)= backtrackingTMCpp_saveToFile tm

loadFromFile :: BacktrackingTM -> FilePath -> IO ()
loadFromFile (BacktrackingTM _ tm)= backtrackingTMCpp_loadFromFile tm
