module Foreign.Nupic.Generator.Types
  ( classifierResultT
  , sdrT
  , typeExports
  ) where

import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Types
import           Foreign.Nupic.Generator.Vector

c_classifierResult :: Class
c_classifierResult =
  addReqIncludes [includeLocal "nupic/types/ClassifierResult.hpp"] $
  makeClass (ident2 "nupic" "types" "ClassifierResult") (Just $ toExtName "ClassifierResult") [] $
  [ mkCtor "new" []
  , mkMethod "getClass" [uintT] uintT
  ]

classifierResultT :: Type
classifierResultT = objT c_classifierResult

c_sdr :: Class
c_sdr =
  addReqIncludes [includeLocal "nupic/types/Sdr.hpp"] $
  makeClass (ident1 "nupic" "SDR") (Just $ toExtName "Sdr") [] $
  [ mkCtor "new" [constUintVectorT]
  , mkMethod "getDense" [] charVectorT'
  , mkMethod "setDense" [charVectorT] voidT
  , mkMethod' "setDense" "setDenseWithUChar" [ucharVectorT] voidT
  , mkMethod "getSparse" [] uintVectorT'
  , mkMethod "setSparse" [uintVectorT] voidT
  ]

sdrT :: Type
sdrT = objT c_sdr

c_sdr_dimensions :: Function
c_sdr_dimensions =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "sdr_dimensions") Nothing Nonpure [sdrT] uintVectorT'

typeExports :: [Export]
typeExports =
  [ ExportClass c_classifierResult
  , ExportClass c_sdr
  , ExportFn c_sdr_dimensions
  ]
