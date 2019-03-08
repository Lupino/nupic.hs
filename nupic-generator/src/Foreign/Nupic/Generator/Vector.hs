module Foreign.Nupic.Generator.Vector
  ( uintVectorT
  , constUintVectorT
  , uintVectorT'
  , charVectorT
  , constCharVectorT
  , charVectorT'
  , ucharVectorT
  , constUCharVectorT
  , ucharVectorT'
  , constReal64VectorT
  , vExports
  ) where

import           Foreign.Hoppy.Generator.Spec       (Export, Type)
import           Foreign.Hoppy.Generator.Std        (ValueConversion (..))
import           Foreign.Hoppy.Generator.Std.Vector (Contents (..),
                                                     Options (..),
                                                     defaultOptions,
                                                     instantiate', toExports)
import           Foreign.Hoppy.Generator.Types      (charT, constT, doubleT,
                                                     objT, objToHeapT, ucharT,
                                                     uintT)

options :: Options
options = defaultOptions  {optValueConversion = Just ConvertValue}

c_uintVector :: Contents
c_uintVector = instantiate' "UIntVector" uintT mempty options

uintVectorT :: Type
uintVectorT      = objT $ c_vector c_uintVector

constUintVectorT :: Type
constUintVectorT = constT uintVectorT

uintVectorT' :: Type
uintVectorT'     = objToHeapT $ c_vector c_uintVector

c_charVector :: Contents
c_charVector = instantiate' "ByteVector" charT mempty options

charVectorT :: Type
charVectorT      = objT $ c_vector c_charVector

constCharVectorT :: Type
constCharVectorT = constT charVectorT

charVectorT' :: Type
charVectorT'     = objToHeapT $ c_vector c_charVector

c_ucharVector :: Contents
c_ucharVector = instantiate' "UByteVector" ucharT mempty options

ucharVectorT :: Type
ucharVectorT      = objT $ c_vector c_ucharVector

constUCharVectorT :: Type
constUCharVectorT = constT ucharVectorT

ucharVectorT' :: Type
ucharVectorT'     = objToHeapT $ c_vector c_ucharVector

c_real64Vector :: Contents
c_real64Vector = instantiate' "Real64Vector" doubleT mempty options

constReal64VectorT :: Type
constReal64VectorT = constT $ objT $ c_vector c_real64Vector

vExports :: [Export]
vExports =
  toExports c_uintVector
  ++ toExports c_charVector
  ++ toExports c_ucharVector
  ++ toExports c_real64Vector
