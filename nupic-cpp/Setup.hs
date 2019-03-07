module Main (main) where

import           Foreign.Hoppy.Setup (ProjectConfig (..), cppMain)

main =
  cppMain
  ProjectConfig
  { generatorExecutableName = "nupic-generator"
  , cppPackageName = "nupic-cpp"
  , cppSourcesDir = "cpp"
  , hsSourcesDir = "src"
  }
