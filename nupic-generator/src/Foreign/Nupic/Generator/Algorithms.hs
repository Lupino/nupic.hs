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
  [ mkCtor "new" [refT constUintVectorT, doubleT, doubleT, uintT]
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
  [ mkCtor "new2" [ constUintVectorT, constUintVectorT ]
  , mkCtor "new"
    [ constUintVectorT, constUintVectorT
    --            UInt potentialRadius = 16u, Real potentialPct = 0.5f,
    , uintT, floatT
    --            bool globalInhibition = true, Real localAreaDensity = DISABLED,
    , boolT, floatT
    --            Int numActiveColumnsPerInhArea = 10u, UInt stimulusThreshold = 0u,
    , intT, uintT
    --            Real synPermInactiveDec = 0.01f, Real synPermActiveInc = 0.1f,
    , floatT, floatT
    --            Real synPermConnected = 0.1f, Real minPctOverlapDutyCycles = 0.001f,
    , floatT, floatT
    --            UInt dutyCyclePeriod = 1000u, Real boostStrength = 0.0f,
    , uintT, floatT
    --            Int seed = 1, UInt spVerbosity = 0u, bool wrapAround = true);
    , intT, uintT, boolT
    ]
   -- virtual void compute(SDR &input, bool learn, SDR &active);
  , mkMethod "compute" [refT sdrT, boolT, refT sdrT] voidT
  , mkMethod "getNumColumns" [] uintT
  , mkMethod "getIterationNum" [] uintT
  , mkMethod "setGlobalInhibition" [boolT] voidT
  , mkMethod "getGlobalInhibition" [] boolT
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

c_cells4 :: Class
c_cells4 =
  addReqIncludes [includeLocal "nupic/algorithms/Cells4.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "Cells4" "Cells4") (Just $ toExtName "Cells4") [] $
    [ mkCtor "new"
      [ uintT, uintT, uintT
      -- Cells4(UInt nColumns = 0, UInt nCellsPerCol = 0, UInt activationThreshold = 1,
      , uintT, uintT
      --        UInt minThreshold = 1, UInt newSynapseCount = 1,
      , uintT, floatT
      --        UInt segUpdateValidDuration = 1, Real permInitial = .5,
      , floatT, floatT, floatT
      --        Real permConnected = .8, Real permMax = 1, Real permDec = .1,
      , floatT, floatT, boolT
      --        Real permInc = .1, Real globalDecay = 0, bool doPooling = false,
      , intT, boolT
      --        int seed = -1, bool initFromCpp = true,
      , boolT
      --        bool checkSynapseConsistency = false);
      ]
  , mkCtor "new0" []
  -- void compute(Real *input, Real *output, bool doInference, bool doLearning);
  , mkMethod "compute" [ptrT floatT, ptrT floatT, boolT, boolT] voidT
  ]

c_anomalyMode :: CppEnum
c_anomalyMode = makeEnum (ident3 "nupic" "algorithms" "anomaly" "AnomalyMode") (Just $ toExtName "AnomalyMode") $
  [ (0, ["PURE"])
  , (1, ["LIKELIHOOD"])
  , (2, ["WEIGHTED"])
  ]

c_anomaly :: Class
c_anomaly =
  addReqIncludes [includeLocal "nupic/algorithms/Anomaly.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "anomaly" "Anomaly") (Just $ toExtName "Anomaly") [] $
    [ mkCtor "new" [ uintT, enumT c_anomalyMode, floatT ]
    , mkMethod "compute" [refT uintVectorT, refT uintVectorT, intT] floatT
    ]

algorithmExports :: [Export]
algorithmExports =
  [ ExportClass c_sdrClassifier
  , ExportFn c_sdrClassifier_save
  , ExportFn c_sdrClassifier_load
  , ExportClass c_spatialPooler
  , ExportFn c_spatialPooler_save
  , ExportFn c_spatialPooler_load
  , ExportClass c_cells4
  , ExportEnum c_anomalyMode
  , ExportClass c_anomaly
  ]
