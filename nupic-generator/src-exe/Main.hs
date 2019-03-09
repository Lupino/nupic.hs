module Main (main) where

import           Foreign.Hoppy.Generator.Main       (defaultMain)
import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Std
import           Foreign.Hoppy.Generator.Types
import           Foreign.Nupic.Generator.Algorithms
import           Foreign.Nupic.Generator.Encoders
import           Foreign.Nupic.Generator.Types
import           Foreign.Nupic.Generator.Vector

main :: IO ()
main = defaultMain interfaceResult

interfaceResult :: Either String Interface
interfaceResult = do
  iface <- interface "nupic"
           [ mod_nupic
           , moduleModify' mod_std $ do
               moduleSetHppPath "gen_std.hpp"
               moduleSetCppPath "gen_std.cpp"
           ]
  interfaceAddHaskellModuleBase ["Foreign", "Nupic"] iface

mod_nupic :: Module
mod_nupic =
  moduleModify' (makeModule "internal" "gen_nupic.hpp" "gen_nupic.cpp") $ do
    moduleAddExports $ vExports ++ typeExports ++ algorithmExports ++ encoderExports
    moduleAddExports
      [ ExportFn c_setup
      , ExportFn c_getTrainImage
      , ExportFn c_getTrainLabel
      , ExportFn c_getTestImage
      , ExportFn c_getTestLabel
      ]

c_setup :: Function
c_setup =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "setup") Nothing Nonpure [] voidT

c_getTrainImage :: Function
c_getTrainImage =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "getTrainImage") Nothing Nonpure [uintT] ucharVectorT'

c_getTrainLabel :: Function
c_getTrainLabel =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "getTrainLabel") Nothing Nonpure [uintT] uintT

c_getTestImage :: Function
c_getTestImage =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "getTestImage") Nothing Nonpure [uintT] ucharVectorT'

c_getTestLabel :: Function
c_getTestLabel =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "getTestLabel") Nothing Nonpure [uintT] uintT
