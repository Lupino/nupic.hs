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

algorithmExports :: [Export]
algorithmExports =
  [ ExportClass c_sdrClassifier
  , ExportClass c_spatialPooler
  , ExportClass c_cells4
  , ExportEnum c_anomalyMode
  , ExportClass c_anomaly
  , ExportClass c_anomalyLikelihood
  ]
