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
  mnist <- mnist_new
  mnist_setup mnist "mnist-src" input columns
  mnist_train mnist input columns
  mnist_test mnist input columns
