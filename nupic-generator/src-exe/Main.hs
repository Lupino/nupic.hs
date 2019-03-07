module Main (main) where

import           Foreign.Hoppy.Generator.Main             (defaultMain)
import           Foreign.Hoppy.Generator.Std              (c_string, mod_std)
import           Foreign.Hoppy.Generator.Types            (objT)

import           Foreign.Hoppy.Generator.Language.Haskell (addExport,
                                                           addImports, indent,
                                                           sayLn)
import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Std.Vector
import           Foreign.Hoppy.Generator.Types

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
    moduleAddExports (toExports c_uintVector)
    moduleAddExports
      [ ExportClass c_mnist
      , ExportClass c_sdr
      , ExportFn c_getSDRDimensions
      ]

c_mnist :: Class
c_mnist =
  addReqIncludes [includeLocal "nupic.hpp"] $
  makeClass (ident1 "nupic" "MNIST") (Just $ toExtName "Mnist") []
  [ mkCtor "new" []
  , mkMethod "setup" [objT c_string] voidT
  , mkMethod "train" [] voidT
  , mkMethod "test" [] voidT
  ]

c_uintVector :: Contents
c_uintVector = instantiate "UIntVector" uintT mempty

c_sdr :: Class
c_sdr =
  addReqIncludes [includeLocal "nupic/types/Sdr.hpp"] $
  makeClass (ident1 "nupic" "SDR") (Just $ toExtName "Sdr") [] $
  [ mkMethod "initialize" [constT $ objT $ c_vector c_uintVector] voidT
  ]

c_getSDRDimensions :: Function
c_getSDRDimensions =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "getSDRDimensions") Nothing Nonpure [objT c_sdr] $ objToHeapT $ c_vector c_uintVector
