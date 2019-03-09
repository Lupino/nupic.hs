module Main (main) where

import           Foreign.Hoppy.Generator.Main   (defaultMain)
import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Std
import           Foreign.Hoppy.Generator.Types
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
    moduleAddExports vExports
    moduleAddExports
      [ ExportClass c_sdr
      , ExportClass c_sdrClassifier
      , ExportFn c_sdrClassifier_save
      , ExportFn c_sdrClassifier_load
      , ExportFn c_getSDRDimensions
      , ExportClass c_classifierResult
      , ExportClass c_spatialPooler
      , ExportFn c_spatialPooler_save
      , ExportFn c_spatialPooler_load
      , ExportFn c_setup
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

c_sdr :: Class
c_sdr =
  addReqIncludes [includeLocal "nupic/types/Sdr.hpp"] $
  makeClass (ident1 "nupic" "SDR") (Just $ toExtName "Sdr") [] $
  [ mkCtor "new" []
  , mkMethod "initialize" [constUintVectorT] voidT
  , mkMethod "getDense" [] charVectorT'
  , mkMethod "setDense" [charVectorT] voidT
  , mkMethod' "setDense" "setDenseWithUChar" [ucharVectorT] voidT
  , mkMethod "getSparse" [] uintVectorT'
  , mkMethod "setSparse" [uintVectorT] voidT
  ]

sdrT :: Type
sdrT = objT c_sdr

c_getSDRDimensions :: Function
c_getSDRDimensions =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "getSDRDimensions") Nothing Nonpure [sdrT] uintVectorT'


c_sdrClassifier :: Class
c_sdrClassifier =
  addReqIncludes [includeLocal "nupic/algorithms/SDRClassifier.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "sdr_classifier" "SDRClassifier") (Just $ toExtName "SdrClassifier") [] $
  [ mkCtor "new" []
  , mkMethod "initialize" [refT constUintVectorT, doubleT, doubleT, uintT] voidT
  , mkMethod "compute"
    [ uintT, refT constUintVectorT
    , refT constUintVectorT
    , refT constReal64VectorT
    , boolT, boolT, boolT
    , refT classifierResultT
    ] voidT
  ]

sdrClassifierT :: Type
sdrClassifierT = objT c_sdrClassifier

c_classifierResult :: Class
c_classifierResult =
  addReqIncludes [includeLocal "nupic/types/ClassifierResult.hpp"] $
  makeClass (ident2 "nupic" "types" "ClassifierResult") (Just $ toExtName "ClassifierResult") [] $
  [ mkCtor "new" []
  , mkMethod "getClass" [uintT] uintT
  ]

classifierResultT :: Type
classifierResultT = objT c_classifierResult

c_spatialPooler :: Class
c_spatialPooler =
  addReqIncludes [includeLocal "nupic/algorithms/SpatialPooler.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "spatial_pooler" "SpatialPooler") (Just $ toExtName "SpatialPooler") [] $
  [ mkCtor "new" []
  , mkMethod "initialize"
    -- const vector<UInt> inputDimensions, const vector<UInt> columnDimensions,
    [ constUintVectorT, constUintVectorT
    --            UInt potentialRadius = 16u, Real potentialPct = 0.5f,
    , uintT, doubleT
    --            bool globalInhibition = true, Real localAreaDensity = DISABLED,
    , boolT, doubleT
    --            Int numActiveColumnsPerInhArea = 10u, UInt stimulusThreshold = 0u,
    , intT, uintT
    --            Real synPermInactiveDec = 0.01f, Real synPermActiveInc = 0.1f,
    , doubleT, doubleT
    --            Real synPermConnected = 0.1f, Real minPctOverlapDutyCycles = 0.001f,
    , doubleT, doubleT
    --            UInt dutyCyclePeriod = 1000u, Real boostStrength = 0.0f,
    , uintT, doubleT
    --            Int seed = 1, UInt spVerbosity = 0u, bool wrapAround = true);
    , intT, uintT, boolT
    ] voidT
   -- virtual void compute(SDR &input, bool learn, SDR &active);
  , mkMethod "compute" [refT sdrT, boolT, refT sdrT] voidT
  , mkMethod "getNumColumns" [] uintT
  , mkMethod "getIterationNum" [] uintT
  ]

spatialPoolerT :: Type
spatialPoolerT = objT c_spatialPooler

c_spatialPooler_save :: Function
c_spatialPooler_save =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "spatialPooler_save") Nothing Nonpure [spatialPoolerT, objT c_string] voidT

c_spatialPooler_load :: Function
c_spatialPooler_load =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "spatialPooler_load") Nothing Nonpure [refT spatialPoolerT, objT c_string] voidT

c_sdrClassifier_save :: Function
c_sdrClassifier_save =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "sdrClassifier_save") Nothing Nonpure [sdrClassifierT, objT c_string] voidT

c_sdrClassifier_load :: Function
c_sdrClassifier_load =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "sdrClassifier_load") Nothing Nonpure [refT sdrClassifierT, objT c_string] voidT
