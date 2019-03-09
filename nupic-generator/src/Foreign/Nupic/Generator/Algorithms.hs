module Foreign.Nupic.Generator.Algorithms
  ( sdrClassifierT
  , spatialPoolerT
  , algorithmExports
  ) where

import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Std
import           Foreign.Hoppy.Generator.Types
import           Foreign.Nupic.Generator.Types
import           Foreign.Nupic.Generator.Vector

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

c_sdrClassifier_save :: Function
c_sdrClassifier_save =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "sdrClassifier_save") Nothing Nonpure [sdrClassifierT, objT c_string] voidT

c_sdrClassifier_load :: Function
c_sdrClassifier_load =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "sdrClassifier_load") Nothing Nonpure [refT sdrClassifierT, objT c_string] voidT

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

algorithmExports :: [Export]
algorithmExports =
  [ ExportClass c_sdrClassifier
  , ExportFn c_sdrClassifier_save
  , ExportFn c_sdrClassifier_load
  , ExportClass c_spatialPooler
  , ExportFn c_spatialPooler_save
  , ExportFn c_spatialPooler_load
  ]
