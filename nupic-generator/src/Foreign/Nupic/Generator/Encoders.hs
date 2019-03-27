module Foreign.Nupic.Generator.Encoders
  ( encoderExports
  ) where

import           Foreign.Hoppy.Generator.Spec
import           Foreign.Hoppy.Generator.Types
import           Foreign.Nupic.Generator.Types  (sdrT)
import           Foreign.Nupic.Generator.Vector


c_scalarEncoder :: Class
c_scalarEncoder =
  addReqIncludes [includeLocal "nupic/encoders/ScalarEncoder.hpp"] $
  makeClass (ident2 "nupic" "encoders" "ScalarEncoder") (Just $ toExtName "ScalarEncoder") [] $
  [ mkMethod "encode" [doubleT, refT sdrT] voidT
  ]

c_scalarEncoder_new :: Function
c_scalarEncoder_new =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "scalarEncoder_new") Nothing Nonpure
    [ doubleT, doubleT
    , boolT, boolT
    , uintT
    , floatT
    , uintT
    , doubleT, doubleT
    ] $ ptrT $ objT c_scalarEncoder

encoderExports :: [Export]
encoderExports =
  [ ExportClass c_scalarEncoder
  , ExportFn c_scalarEncoder_new
  ]
