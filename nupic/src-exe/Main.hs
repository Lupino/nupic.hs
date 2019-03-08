module Main (main) where

import qualified Foreign.Hoppy.Runtime  as FHR
import           Foreign.Nupic.Internal
import           Foreign.Nupic.Std      ()
import           Prelude
import           System.Random          (newStdGen)
import           System.Random.Shuffle  (shuffle')

main :: IO ()
main = do
  input <- sdr_new
  columns <- sdr_new
  v <- FHR.fromContents [28, 28]  :: IO UIntVector
  sdr_initialize input v

  clsr <- sdrClassifier_new
  v1 <- FHR.fromContents [0] :: IO UIntVector
  sp <- spatialPooler_new

  dims <- getSDRDimensions input

  spatialPooler_initialize sp dims v 5 0.5 False 0.20 (-1) 6 0.005 0.01 0.4 0.001 1402 2.5 93 1 False

  numColums <- spatialPooler_getNumColumns sp

  vv <- FHR.fromContents [numColums] :: IO UIntVector

  sdr_initialize columns vv
  sdrClassifier_initialize clsr v1 0.001 3 1

  setup

  -- trainIdxs <- getTrainIdxs
  -- trainIdxs' <- FHR.toContents trainIdxs :: IO [F.CUInt]

  putStrLn $ "Start training"
  rng <- newStdGen

  flip mapM_ (shuffle' [0..59999] 60000 rng) $ \idx -> do
    image <- getTrainImage idx
    label <- getTrainLabel idx
    sdr_setDenseWithUChar input image
    spatialPooler_compute sp input True columns
    result <- classifierResult_new
    iterNum <- spatialPooler_getIterationNum sp
    sparse <- sdr_getSparse columns
    labelv <- FHR.fromContents [label] :: IO UIntVector
    labelv1 <- FHR.fromContents [fromIntegral label] :: IO Real64Vector
    sdrClassifier_compute clsr iterNum sparse labelv labelv1 True True False result

  putStrLn "End training"

  -- testIdxs <- getTestIdxs
  -- testIdxs' <- FHR.toContents testIdxs :: IO [F.CUInt]

  putStrLn $ "Start testing" -- ++ show (length testIdxs')
  r <- flip mapM [0..9999] $ \idx -> do
    image <- getTestImage idx
    label <- getTestLabel idx
    sdr_setDenseWithUChar input image
    spatialPooler_compute sp input False columns
    result <- classifierResult_new
    iterNum <- spatialPooler_getIterationNum sp
    sparse <- sdr_getSparse columns
    l <- uIntVector_new
    lv <- real64Vector_new
    sdrClassifier_compute clsr iterNum sparse l lv True False True result
    res <- classifierResult_getClass result 0
    return $ res == label
  putStrLn "End testing"

  print $ fromIntegral (sum (map (\vr -> if vr then 1 else 0) r)) / fromIntegral (length r)
