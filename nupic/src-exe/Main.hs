module Main (main) where

import qualified Foreign                as F
import qualified Foreign.C              as F
import qualified Foreign.Hoppy.Runtime  as FHR
import           Foreign.Nupic.Internal
import           Foreign.Nupic.Std      ()
import           Prelude

main :: IO ()
main = do
  input <- sdr_new
  columns <- sdr_new
  v <- FHR.fromContents [28, 28]  :: IO UIntVector
  sdr_initialize input v

  clsr <- sdrClassifier_new
  v1 <- FHR.fromContents [0] :: IO UIntVector
  mnist <- mnist_new
  mnist_setup mnist "mnist-src" input columns
  sdrClassifier_initialize clsr v1 0.001 3 1
  mnist_train mnist input columns clsr
  mnist_test mnist input columns clsr
