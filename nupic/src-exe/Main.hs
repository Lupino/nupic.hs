module Main (main) where

import           Foreign.Nupic.Internal (mnist_new, mnist_setup, mnist_test,
                                         mnist_train)
import           Foreign.Nupic.Std      ()
import           Prelude

main :: IO ()
main = do
  mnist <- mnist_new
  mnist_setup mnist "mnist-src"
  mnist_train mnist
  mnist_test mnist
