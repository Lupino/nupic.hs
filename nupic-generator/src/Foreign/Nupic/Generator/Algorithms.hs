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
  , mkMethod "saveToFile" [objT c_string] voidT
  , mkMethod "loadFromFile" [objT c_string] voidT
  ]

sdrClassifierT :: Type
sdrClassifierT = objT c_sdrClassifier

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
  , mkMethod' "compute" "computeWithPtr" [ptrT uintT, boolT, ptrT uintT] voidT
  , mkMethod "getNumColumns" [] uintT
  , mkMethod "getIterationNum" [] uintT
  , mkMethod "setGlobalInhibition" [boolT] voidT
  , mkMethod "getGlobalInhibition" [] boolT
  , mkMethod "saveToFile" [objT c_string] voidT
  , mkMethod "loadFromFile" [objT c_string] voidT
  ]

spatialPoolerT :: Type
spatialPoolerT = objT c_spatialPooler

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
  -- , mkMethod "nSegments"                  [] uintT
  , mkMethod "nCells"                     [] uintT
  , mkMethod "nColumns"                   [] uintT
  , mkMethod "nCellsPerCol"               [] uintT
  -- , mkMethod "getPermInitial"             [] floatT
  -- , mkMethod "getMinThreshold"            [] uintT
  -- , mkMethod "getPermConnected"           [] floatT
  -- , mkMethod "getNewSynapseCount"         [] uintT
  -- , mkMethod "getPermInc"                 [] floatT
  -- , mkMethod "getPermDec"                 [] floatT
  -- , mkMethod "getPermMax"                 [] floatT
  -- , mkMethod "getGlobalDecay"             [] floatT
  -- , mkMethod "getActivationThreshold"     [] uintT
  -- , mkMethod "getDoPooling"               [] boolT
  -- , mkMethod "getSegUpdateValidDuration"  [] uintT
  -- , mkMethod "getVerbosity"               [] uintT
  -- , mkMethod "getMaxAge"                  [] uintT
  -- , mkMethod "getPamLength"               [] uintT
  -- , mkMethod "getMaxInfBacktrack"         [] uintT
  -- , mkMethod "getMaxLrnBacktrack"         [] uintT
  -- , mkMethod "getPamCounter"              [] uintT
  -- , mkMethod "getMaxSeqLength"            [] uintT
  -- , mkMethod "getAvgLearnedSeqLength"     [] floatT
  -- , mkMethod "getNLrnIterations"          [] uintT
  -- , mkMethod "getMaxSegmentsPerCell"      [] intT
  -- , mkMethod "getMaxSynapsesPerSegment"   [] intT
  -- , mkMethod "getCheckSynapseConsistency" [] boolT
  , mkMethod "saveToFile" [objT c_string] voidT
  , mkMethod "loadFromFile" [objT c_string] voidT
  ]

cells4T :: Type
cells4T = objT c_cells4

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

c_anomalyLikelihood :: Class
c_anomalyLikelihood =
  addReqIncludes [includeLocal "nupic/algorithms/AnomalyLikelihood.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "anomaly" "AnomalyLikelihood") (Just $ toExtName "AnomalyLikelihood") [] $
    [ mkCtor "new" [ uintT, uintT, uintT, uintT, uintT ]
    , mkCtor "new0" []
    , mkMethod "anomalyProbability" [floatT, intT] floatT
    ]

c_temporalMemory :: Class
c_temporalMemory =
  addReqIncludes [includeLocal "nupic/algorithms/TemporalMemory.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "temporal_memory" "TemporalMemory") (Just $ toExtName "TemporalMemory") [] $
    [ mkCtor "new"
      [ uintVectorT, uintT, uintT, floatT, floatT, uintT, uintT, floatT, floatT
      , floatT, intT, uintT, uintT, boolT, uintT
      ]
    , mkMethod "version" [] uintT
    , mkMethod "seed_" [longT] voidT
    , mkMethod "reset" [] voidT
    , mkMethod "compute" [refT sdrT, boolT, refT sdrT, refT sdrT] voidT
    , mkMethod "getActiveCells" [] uintVectorT'
    , mkMethod "getPredictiveCells" [] uintVectorT'
    , mkMethod "getWinnerCells" [] uintVectorT'
    , mkMethod "saveToFile" [objT c_string] voidT
    , mkMethod "loadFromFile" [objT c_string] voidT
    , mkMethod "numberOfCells" [] uintT
    , mkMethod "activateDendrites" [boolT, refT uintVectorT, refT uintVectorT] voidT
    ]

c_backtrackingTM :: Class
c_backtrackingTM =
  addReqIncludes [includeLocal "nupic/algorithms/BacktrackingTM.hpp"] $
  makeClass (ident3 "nupic" "algorithms" "backtracking_tm" "BacktrackingTM") (Just $ toExtName "BacktrackingTM") [] $
    [ mkCtor "new"
      [ uintT
      -- BacktrackingTM(UInt32 numberOfCols,
      , uintT
      --                   UInt32 cellsPerColumn, // first two fields are required
      , floatT, floatT
      --                   Real32 initialPerm = 0.11f, Real32 connectedPerm = 0.50f,
      , uintT, uintT
      --                   UInt32 minThreshold = 8, UInt32 newSynapseCount = 15,
      , floatT, floatT
      --                   Real32 permanenceInc = 0.10f, Real32 permanenceDec = 0.10f,
      , floatT, floatT
      --                   Real32 permanenceMax = 1.0f, Real32 globalDecay = 0.10f,
      , uintT, boolT
      --                   UInt32 activationThreshold = 12, bool doPooling = false,
      , uintT, uintT
      --                   UInt32 segUpdateValidDuration = 5, UInt32 burnIn = 2,
      , boolT, intT
      --                   bool collectStats = false, Int32 seed = 42,
      , intT, boolT
      --                   Int32 verbosity = 0, bool checkSynapseConsistency = false,
      , uintT, uintT
      --                   UInt32 pamLength = 1, UInt32 maxInfBacktrack = 10,
      , uintT, uintT
      --                   UInt32 maxLrnBacktrack = 5, UInt32 maxAge = 100000,
      , uintT, intT
      --                   UInt32 maxSeqLength = 32, Int32 maxSegmentsPerCell = -1,
      , intT
      --                   Int32 maxSynapsesPerSegment = -1,
      , objT c_string
      --                   const std::string outputType = "normal");
      ]
    , mkMethod "version" [] uintT
    , mkMethod "compute" [ptrT floatT, boolT, boolT] $ ptrT floatT
    , mkMethod "finishLearning" [] voidT
    , mkMethod "reset" [] voidT
    , mkMethod "saveToFile" [objT c_string] voidT
    , mkMethod "loadFromFile" [objT c_string] voidT
    ]

backTMT :: Type
backTMT = objT c_backtrackingTM

c_backTM_predict :: Function
c_backTM_predict =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "backTM_predict") Nothing Nonpure [refT backTMT, uintT] $ ptrT floatT

algorithmExports :: [Export]
algorithmExports =
  [ ExportClass c_sdrClassifier
  , ExportClass c_spatialPooler
  , ExportClass c_cells4
  , ExportEnum c_anomalyMode
  , ExportClass c_anomaly
  , ExportClass c_anomalyLikelihood
  , ExportClass c_temporalMemory
  , ExportClass c_backtrackingTM
  , ExportFn c_backTM_predict
  ]
