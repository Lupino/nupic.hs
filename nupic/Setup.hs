module Main (main) where

import           Foreign.Hoppy.Setup (ProjectConfig (..), hsMain)

main =
  hsMain
  ProjectConfig
  { generatorExecutableName = "nupic-generator"
  , cppPackageName = "nupic-cpp"
  , cppSourcesDir = "cpp"
  , hsSourcesDir = "src"
  }
