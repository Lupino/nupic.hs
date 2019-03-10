module Main (main) where

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (foldM, replicateM, void)
import           Foreign.Nupic.Internal
import           Foreign.Nupic.Std              ()
import           Prelude
import           System.Random                  (getStdRandom, newStdGen,
                                                 randomR)
import           System.Random.Shuffle          (shuffle')

import qualified Nupic.Algorithms.Anomaly       as AN
import qualified Nupic.Algorithms.Cells4        as TP
import qualified Nupic.Algorithms.SdrClassifier as CLSR
import qualified Nupic.Algorithms.SpatialPooler as SP
import qualified Nupic.Encoders.ScalarEncoder   as SE
import           Nupic.Types                    (getClass, getDimensions,
                                                 getSparse, newSdr)

mnist :: IO ()
mnist = do
  input <- newSdr [28, 28]
  clsr <- CLSR.new [0] 0.001 3 1
  dims <- getDimensions input
  sp <- SP.new $ SP.Options dims [28, 28] 5 0.5 False 0.20 (-1) 6 0.005 0.01 0.4 0.001 1402 2.5 93 1 False
  numColums <- SP.getNumColumns sp
  columns <- newSdr [numColums]

  setup

  putStrLn $ "Start training"
  rng <- newStdGen

  flip mapM_ (shuffle' [0..59999] 60000 rng) $ \idx -> do
    image <- getTrainImage idx
    label <- getTrainLabel idx
    sdr_setDenseWithUChar input image
    SP.compute sp input True columns
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
    SP.compute sp input False columns
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
  enc <- SE.new 133 (-100) 100.0 dim_input 0.0 0.0 False

  spGlobal <- SP.new $ (SP.options [fromIntegral dim_input] [cols]) {SP.globalInhibition = True}

  tp <- TP.new $ TP.Options cols cells 12 8 15 5 0.5 0.8 1.0 0.1 0.1 0.0 False 42 True False
  an <- AN.new 5 AN.Likelihood 0
  -- anLikelihood <- anomaly_new 5 AnomalyMode_Likelihood 0

  ncells <- fromIntegral <$> TP.nCells tp
  let rOutI = replicate ncells 0.0

  let train prevPred val = do
        input <- SE.encode enc val
        rIn <- SP.compute_ spGlobal input True $ replicate (fromIntegral cols) 0
        rOut <- TP.compute tp (map fromIntegral rIn) rOutI True True
        res <- AN.compute an rIn prevPred
        print res
        threadDelay 1000000
        return $ map floor rOut

  values <- replicateM 100 $ getStdRandom (randomR (-100.0,100.0)) :: IO [Float]
  r <- foldM train (replicate (fromIntegral ncells) 0) values
  print r
  putStrLn "SaveFile"
  TP.saveToFile tp "cells4.txt"

main :: IO ()
main = hotsp
