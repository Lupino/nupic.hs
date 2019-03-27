module Main (main) where

import           Control.Concurrent              (threadDelay)
import           Control.Monad                   (foldM, replicateM,
                                                  replicateM_, void, when)
import           Foreign.C                       (CFloat, CUInt)
import           Foreign.Nupic.Internal
import           Foreign.Nupic.Std               ()
import           Prelude
import           System.Random                   (getStdRandom, newStdGen,
                                                  randomR)
import           System.Random.Shuffle           (shuffle')

import qualified Nupic.Algorithms.Anomaly        as AN
import qualified Nupic.Algorithms.BacktrackingTM as BackTM
import qualified Nupic.Algorithms.Cells4         as TP
import qualified Nupic.Algorithms.SdrClassifier  as CLSR
import qualified Nupic.Algorithms.SpatialPooler  as SP
import qualified Nupic.Algorithms.TemporalMemory as TM
import qualified Nupic.Encoders                  as E
import qualified Nupic.Encoders.ScalarEncoder    as SE
import           Nupic.Types                     (binaryToSparse, getClass,
                                                  getDense, getDimensions,
                                                  getSparse, newSdr, setDense,
                                                  setSparse, sparseToBinary)

mnist :: IO ()
mnist = do
  input <- newSdr [28, 28]
  clsr <- CLSR.new [0] 0.001 3 1
  dims <- getDimensions input
  sp <- SP.new $ SP.Options dims [28, 28] 5 0.5 False 0.20 (-1) 6 0.005 0.01 0.4 0.001 1402 2.5 93 1 False

  setup

  putStrLn $ "Start training"
  rng <- newStdGen

  flip mapM_ (shuffle' [0..59999] 60000 rng) $ \idx -> do
    image <- getTrainImage idx
    label <- getTrainLabel idx
    sdr_setDenseWithUChar input image
    columns <- SP.compute_ sp input True
    iterNum <- SP.getIterationNum sp
    sparse <- getSparse columns
    void $ CLSR.compute clsr iterNum sparse [label] [fromIntegral label] True True False

  putStrLn "End training"

  SP.saveToFile sp "spatial_pooler.txt"
  CLSR.saveToFile clsr "sdr_calssifier.txt"
  -- SP.loadFromFile sp "spatial_pooler.txt"
  -- CLSR.loadFromFile clsr "sdr_calssifier.txt"

  putStrLn $ "Start testing"
  r <- flip mapM [0..9999] $ \idx -> do
    image <- getTestImage idx
    label <- getTestLabel idx
    sdr_setDenseWithUChar input image
    columns <- SP.compute_ sp input False
    iterNum <- SP.getIterationNum sp
    sparse <- getSparse columns
    result <- CLSR.compute clsr iterNum sparse [] [] True False True
    res <- getClass result 0
    return $ res == label
  putStrLn "End testing"

  print $ fromIntegral (sum (map (\vr -> if vr then 1 else 0) r)) / fromIntegral (length r)

hotsp :: IO ()
hotsp = do
  let dim_input = 10000
      cols = 2048
      cells = 10

  enc <- SE.new $ SE.defaultParameters
    { SE.activeBits = 133
    , SE.minimum = (-100.0)
    , SE.maximum = 100.0
    , SE.size = dim_input
    }

  spGlobal <- SP.new $ (SP.options [fromIntegral dim_input] [cols]) {SP.globalInhibition = True}

  tp <- TP.new $ TP.Options cols cells 12 8 15 5 0.5 0.8 1.0 0.1 0.1 0.0 False 42 True False

  tm <- TM.new (TM.options [cols*cells]) {TM.cellsPerColumn = cells}

  an <- AN.new 5 AN.Pure 0
  -- anLikelihood <- anomaly_new 5 AnomalyMode_Likelihood 0

  ncells <- fromIntegral <$> TP.nCells tp

  tmCols <- TM.numberOfCells tm
  rOut1S <- newSdr [tmCols]

  encSdr <- newSdr [dim_input]

  let train prevPred val = do
        SE.encode enc val encSdr
        input <- map fromIntegral <$> getDense encSdr
        rIn <- SP.compute spGlobal input True
        rOut <- TP.compute tp (map fromIntegral rIn) True True
        -- TM.compute tm (map floor rOut) True

        -- TM.activateDendrites' tm
        -- rOut1 <- TM.getPredictiveCells tm
        -- print rOut1
        -- setSparse rOut1S rOut1
        -- rOut1' <- getDense rOut1S

        res <- AN.compute an rIn $ map fromIntegral prevPred
        print res
        threadDelay 10000
        -- return $ map fromIntegral rOut1'
        return prevPred

  values <- replicateM 5000 $ getStdRandom (randomR (-100.0,100.0)) :: IO [Double]
  r <- foldM train (replicate (fromIntegral ncells) 0) values
  print r
  putStrLn "SaveFile"
  TP.saveToFile tp "cells4.txt"

cleanZero :: [CFloat] -> [CFloat]
cleanZero = clean
  where clean :: [CFloat] -> [CFloat]
        clean = filter (/=0)

toStr :: [CFloat] -> String
toStr = map go
  where go :: CFloat -> Char
        go f = if f /= 0 then '1' else '.'

cpu :: IO ()
cpu = do
  let enc = E.scalarEncoder 21 0.0 100.0 200 False
  sp <- SP.new $ (SP.options [200] [2048])
    { SP.globalInhibition = True
    , SP.spVerbosity = 0
    , SP.numActiveColumnsPerInhArea = 40
    , SP.seed = 1956
    , SP.potentialPct = 0.5
    , SP.synPermConnected = 0.1
    , SP.synPermActiveInc = 0.1
    , SP.synPermInactiveDec = 0.01
    }

  tm <- BackTM.new (BackTM.options 2048 32)
    { BackTM.seed = 1960
    , BackTM.newSynapseCount = 20
    , BackTM.maxSynapsesPerSegment = 32
    , BackTM.maxSegmentsPerCell = 128
    , BackTM.initialPerm = 0.21
    , BackTM.permanenceInc = 0.1
    , BackTM.permanenceDec = 0.1
    , BackTM.globalDecay = 0.0
    , BackTM.maxAge = 0
    , BackTM.minThreshold = 12
    , BackTM.activationThreshold = 16
    , BackTM.outputType = "normal"
    , BackTM.pamLength = 1
    }

  cl <- CLSR.new [0..5] 0.0001 3 1

  let train val = do
        let (bucketId, input) = E.encode enc val
        tmIn <- SP.compute sp input True

        sdr <- newSdr [2048]
        setDense sdr $ map fromIntegral tmIn
        sparse <- getSparse sdr

        iterNum <- SP.getIterationNum sp
        void $ CLSR.compute cl iterNum sparse [fromIntegral bucketId] [val] True True False

        tmOut <- BackTM.compute tm (map fromIntegral tmIn) True True

        preded <- BackTM.predict tm 5

        when (length preded > 0) $
          flip mapM_ preded $ \v -> do
            let v' = cleanZero v
            when (length v' > 0) $ do
              result <- CLSR.compute cl iterNum sparse [] [] True False True
              res <- fromIntegral <$> getClass result 5

              putStrLn $ "Current: " ++ show val ++ " Pridect: " ++ show (E.decodeWithBucket enc res)


  replicateM_ 5000 $ do
    v <- getStdRandom (randomR (0.0,100.0)) :: IO Double
    train v
    threadDelay 1000000

hello_TM :: IO ()
hello_TM = do
  let charA = [0..9]
      charB = [10..19]
      charC = [20..29]
      charD = [30..39]
      charE = [40..49]
      charF = [50..59]
      charG = [60..69]
      charH = [70..79]
      charI = [80..89]
      charJ = [90..99]

      chars = [charA, charB, charC, charD, charE, charF, charG, charH, charI, charJ]

  tm <- TM.new (TM.options [100])
    { TM.cellsPerColumn = 2
    , TM.initialPermanence = 0.5
    , TM.connectedPermanence = 0.5
    , TM.minThreshold = 8
    , TM.maxNewSynapseCount = 20
    , TM.permanenceIncrement = 0.1
    , TM.permanenceDecrement = 0.0
    , TM.activationThreshold = 8
    }

  replicateM_ 1000 $ do
    TM.reset tm
    flip mapM_ chars $ \c -> do
      putStrLn ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
      TM.compute' tm c True
      print =<< TM.getActiveCells tm
      TM.activateDendrites' tm
      print =<< TM.getPredictiveCells tm
      print =<< TM.getWinnerCells tm

  TM.reset tm

  flip mapM_ chars $ \c -> do
    putStrLn ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    TM.compute' tm c False
    print =<< TM.getActiveCells tm
    TM.activateDendrites' tm
    print =<< TM.getPredictiveCells tm
    print =<< TM.getWinnerCells tm



main :: IO ()
main = do
  -- mnist
  -- hotsp
  cpu
  -- hello_TM
