module Foreign.Nupic.Generator.Encoders
  ( encoderExports
  ) where

import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Types
import           Foreign.Nupic.Generator.Vector


c_scalarEncoderBase :: String -> [ClassEntity] -> Class
c_scalarEncoderBase name other =
  addReqIncludes [includeLocal "nupic/encoders/ScalarEncoder.hpp"] $
  makeClass (ident1 "nupic" name) (Just $ toExtName name) [] $
  [ mkMethod "encodeIntoArray" [floatT, ptrT uintT] intT
  , mkMethod "getOutputWidth" [] uintT
  , mkMethod "encode" [uintT] uintVectorT'
  ] ++ other

encoderExports :: [Export]
encoderExports =
  [ ExportClass $ c_scalarEncoderBase "ScalarEncoder"
    [ mkCtor "new" [intT, doubleT, doubleT, intT, doubleT, doubleT, boolT]
    ]
  , ExportClass $ c_scalarEncoderBase "PeriodicScalarEncoder"
    [ mkCtor "new" [intT, doubleT, doubleT, intT, doubleT, doubleT]
    ]
  ]
