module Main (main) where

import           Control.Monad          (foldM, replicateM, void)
import           Foreign.C              (CFloat, CUInt)
import           Foreign.Hoppy.Runtime  (fromContents)
import           Foreign.Marshal.Array  (newArray, peekArray)
import           Foreign.Nupic.Internal
import           Foreign.Nupic.Std      ()
import           Foreign.Ptr            (Ptr)
import           Prelude
import           System.Random          (getStdRandom, newStdGen, randomR)
import           System.Random.Shuffle  (shuffle')

mnist :: IO ()
mnist = do
  input <- sdr_new
  columns <- sdr_new
  v <- fromContents [28, 28]  :: IO UIntVector
  sdr_initialize input v

  v1 <- fromContents [0] :: IO UIntVector
  clsr <- sdrClassifier_new v1 0.001 3 1

  dims <- sdr_dimensions input

  sp <- spatialPooler_new dims v 5 0.5 False 0.20 (-1) 6 0.005 0.01 0.4 0.001 1402 2.5 93 1 False

  numColums <- spatialPooler_getNumColumns sp

  vv <- fromContents [numColums] :: IO UIntVector

  sdr_initialize columns vv

  setup

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
    labelv <- fromContents [label] :: IO UIntVector
    labelv1 <- fromContents [fromIntegral label] :: IO Real64Vector
    sdrClassifier_compute clsr iterNum sparse labelv labelv1 True True False result

  putStrLn "End training"

  spatialPooler_save sp "sp.data"
  sdrClassifier_save clsr "clsr.data"
  -- spatialPooler_load sp "sp.data"
  -- sdrClassifier_load clsr "clsr.data"

  putStrLn $ "Start testing"
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

main :: IO ()
main = do
  let dim_input = 10000
      cols = 2048
      cells = 10
  enc <- scalarEncoder_new 133 (-100) 100.0 dim_input 0.0 0.0 False
  vdim <- fromContents [fromIntegral dim_input] :: IO UIntVector
  vcols <- fromContents [cols] :: IO UIntVector
  spGlobal <- spatialPooler_new2 vdim vcols
  spatialPooler_setGlobalInhibition spGlobal True

  tp <- cells4_new cols cells 12 8 15 5 0.5 0.8 1.0 0.1 0.1 0.0 False 42 True False
  an <- anomaly_new 5 AnomalyMode_Likelihood 0
  -- anLikelihood <- anomaly_new 5 AnomalyMode_Likelihood 0

  ncells <- cells4_nCells tp

  let train prevPred val = do
        ptrInput <- newArray $ replicate (fromIntegral dim_input) 0 :: IO (Ptr CUInt)
        void $ scalarEncoder_encodeIntoArray enc val ptrInput

        ptrOutSP <- newArray $ replicate (fromIntegral cols) 0 :: IO (Ptr CUInt)
        spatialPooler_computeWithPtr spGlobal ptrInput True ptrOutSP

        rInValues <- map fromIntegral <$> peekArray (fromIntegral cols) ptrOutSP

        ptrRIn <- newArray rInValues :: IO (Ptr CFloat)
        ptrROut <- newArray $ replicate (fromIntegral ncells) 0.0 :: IO (Ptr CFloat)

        cells4_compute tp ptrRIn ptrROut True True
        rOutValues <- map floor <$> peekArray (fromIntegral ncells) ptrROut

        prevPred_ <- fromContents prevPred :: IO UIntVector

        outSPValues <- peekArray (fromIntegral cols) ptrOutSP

        outSP <- fromContents outSPValues :: IO UIntVector

        res <- anomaly_compute an outSP prevPred_ (-1)
        print res
        return rOutValues

  values <- replicateM 5000 $ getStdRandom (randomR (-100.0,100.0)) :: IO [Float]
  r <- foldM train (replicate (fromIntegral ncells) 0) values
  print r
