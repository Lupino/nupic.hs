module Main (main) where

import           Foreign.Hoppy.Generator.Language.Haskell (addExport,
                                                           addImports, indent,
                                                           sayLn)
import           Foreign.Hoppy.Generator.Main             (defaultMain)
import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Std
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
  , mkMethod "setup" [objT c_string, ptrT $ objT c_sdr, ptrT $ objT c_sdr] voidT
  , mkMethod "train" [ptrT $ objT c_sdr, ptrT $ objT c_sdr] voidT
  , mkMethod "test" [ptrT $ objT c_sdr, ptrT $ objT c_sdr] voidT
  ]

c_uintVector :: Contents
c_uintVector = instantiate' "UIntVector" uintT mempty $ defaultOptions {optValueConversion = Just ConvertValue}

uintVectorT      = objT $ c_vector c_uintVector
constUintVectorT = constT uintVectorT
uintVectorT'     = objToHeapT $ c_vector c_uintVector

c_sdr :: Class
c_sdr =
  addReqIncludes [includeLocal "nupic/types/Sdr.hpp"] $
  makeClass (ident1 "nupic" "SDR") (Just $ toExtName "Sdr") [] $
  [ mkCtor "new" []
  , mkMethod "initialize" [constUintVectorT] voidT
  , mkMethod "getDense" [] charVectorT'
  , mkMethod "setDense" [charVectorT] voidT
  , mkMethod "getSparse" [] uintVectorT'
  , mkMethod "setSparse" [uintVectorT] voidT
  ]

c_getSDRDimensions :: Function
c_getSDRDimensions =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "getSDRDimensions") Nothing Nonpure [objT c_sdr] $ objToHeapT $ c_vector c_uintVector
