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
    moduleAddExports (toExports c_charVector)
    moduleAddExports
      [ ExportClass c_mnist
      , ExportClass c_sdr
      , ExportClass c_sdrClassifier
      , ExportClass c_classifierResult
      , ExportFn c_getSDRDimensions
      ]

c_mnist :: Class
c_mnist =
  addReqIncludes [includeLocal "nupic.hpp"] $
  makeClass (ident1 "nupic" "MNIST") (Just $ toExtName "Mnist") []
  [ mkCtor "new" []
  , mkMethod "setup" [objT c_string, ptrT $ objT c_sdr, ptrT $ objT c_sdr] voidT
  , mkMethod "train" [ptrT $ objT c_sdr, ptrT $ objT c_sdr, refT $ objT c_sdrClassifier] voidT
  , mkMethod "test"  [ptrT $ objT c_sdr, ptrT $ objT c_sdr, refT $ objT c_sdrClassifier] voidT
  ]

c_uintVector :: Contents
c_uintVector = instantiate' "UIntVector" uintT mempty $ defaultOptions {optValueConversion = Just ConvertValue}

uintVectorT      = objT $ c_vector c_uintVector
constUintVectorT = constT uintVectorT
uintVectorT'     = objToHeapT $ c_vector c_uintVector

c_charVector :: Contents
c_charVector = instantiate' "ByteVector" charT mempty $ defaultOptions {optValueConversion = Just ConvertValue}

charVectorT      = objT $ c_vector c_charVector
constCharVectorT = constT charVectorT
charVectorT'     = objToHeapT $ c_vector c_charVector

c_real64Vector :: Contents
c_real64Vector = instantiate' "ByteVector" doubleT mempty $ defaultOptions {optValueConversion = Just ConvertValue}

constReal64VectorT = constT $ objT $ c_vector c_real64Vector

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


c_sdrClassifier :: Class
c_sdrClassifier =
  addReqIncludes [includeLocal "nupic/algorithms/SDRClassifier.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "sdr_classifier" "SDRClassifier") (Just $ toExtName "SdrClassifier") [] $
  [ mkCtor "new" []
  , mkMethod "initialize" [refT constUintVectorT, doubleT, doubleT, uintT] voidT
  -- virtual void compute(UInt recordNum, const vector<UInt> &patternNZ,
  --                      const vector<UInt> &bucketIdxList,
  --                      const vector<Real64> &actValueList, bool category,
  --                      bool learn, bool infer, ClassifierResult &result);
  --                    recordNum patternNZ      bucketIdxList     actValueList        category learn infer
  , mkMethod "compute" [uintT, refT constUintVectorT, refT constUintVectorT, refT constReal64VectorT, boolT, boolT, boolT, refT $ objT $ c_classifierResult] voidT
  -- , mkMethod "getDense" [] $ objToHeapT $ c_vector c_charVector
  -- , mkMethod "setDense" [objT $ c_vector c_charVector] voidT
  -- , mkMethod "getSparse" [] $ objToHeapT $ c_vector c_uintVector
  -- , mkMethod "setSparse" [objT $ c_vector c_uintVector] voidT
  ]

c_classifierResult :: Class
c_classifierResult =
  addReqIncludes [includeLocal "nupic/types/ClassifierResult.hpp"] $
  makeClass (ident2 "nupic" "types" "ClassifierResult") (Just $ toExtName "ClassifierResult") [] $
  [ mkCtor "new" []
  -- , mkMethod "initialize" [constUintVectorT, doubleT, doubleT, uintT] voidT
  -- virtual void compute(UInt recordNum, const vector<UInt> &patternNZ,
  --                      const vector<UInt> &bucketIdxList,
  --                      const vector<Real64> &actValueList, bool category,
  --                      bool learn, bool infer, ClassifierResult &result);
  -- , mkMethod "compute" [uintT, constUintVectorT, constUintVectorT, constReal64VectorT, boolT, boolT, boolT, c_classifierResult]
  -- , mkMethod "getDense" [] $ objToHeapT $ c_vector c_charVector
  -- , mkMethod "setDense" [objT $ c_vector c_charVector] voidT
  -- , mkMethod "getSparse" [] $ objToHeapT $ c_vector c_uintVector
  -- , mkMethod "setSparse" [objT $ c_vector c_uintVector] voidT
  ]
