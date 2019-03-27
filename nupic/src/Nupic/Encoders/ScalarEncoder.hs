{-# LANGUAGE RecordWildCards #-}
module Nupic.Encoders.ScalarEncoder
  ( new
  , encode
  , defaultParameters
  , Parameters (..)
  ) where

import           Foreign.C              (CUInt)
import           Foreign.Nupic.Internal (ScalarEncoder, Sdr,
                                         scalarEncoder_encode,
                                         scalarEncoder_new)
import           Prelude                hiding (maximum, minimum)

data Parameters = Parameters
  {
    -- Members "minimum" and "maximum" define the range of the input signal.
    -- These endpoints are inclusive.
    minimum    :: Double
  , maximum    :: Double

    -- Member "clipInput" determines whether to allow input values outside the
    -- range [minimum, maximum].
    -- If true, the input will be clipped into the range [minimum, maximum].
    -- If false, inputs outside of the range will raise an error.
  , clipInput  :: Bool

    -- Member "periodic" controls what happens near the edges of the input
    -- range.
    --
    -- If true, then the minimum & maximum input values are adjacent and the
    -- first and last bits of the output SDR are also adjacent.  The contiguous
    -- block of 1's wraps around the end back to the begining.
    --
    -- If false, then minimum & maximum input values are the endpoints of the
    -- input range, are not adjacent, and activity does not wrap around.
  , periodic   :: Bool

    -- Member "activeBits" is the number of true bits in the encoded output SDR.
    -- The output encodings will have a contiguous block of this many 1's.
  , activeBits :: CUInt

    -- Member "sparsity" is an alternative way to specify the member "activeBits".
    -- Sparsity requires that the size to also be specified.
    -- Specify only one of: activeBits or sparsity.
  , sparsity   :: Float

    -- These three (3) members define the total number of bits in the output:
    --      size,
    --      radius,
    --      resolution.
    --
    -- These are mutually exclusive and only one of them should be non-zero when
    -- constructing the encoder.

    -- Member "size" is the total number of bits in the encoded output SDR.
  , size       :: CUInt

    -- Member "radius" Two inputs separated by more than the radius have
    -- non-overlapping representations. Two inputs separated by less than the
    -- radius will in general overlap in at least some of their bits. You can
    -- think of this as the radius of the input.
  , radius     :: Double

    -- Member "resolution" Two inputs separated by greater than, or equal to the
    -- resolution are guaranteed to have different representations.
  , resolution :: Double
  }

defaultParameters :: Parameters
defaultParameters = Parameters
  { minimum    = 0.0
  , maximum    = 0.0
  , clipInput  = False
  , periodic   = False
  , activeBits = 0
  , sparsity   = 0.0
  , size       = 0
  , radius     = 0.0
  , resolution = 0.0
  }

new :: Parameters -> IO ScalarEncoder
new Parameters {..} =
  scalarEncoder_new
    minimum
    maximum
    clipInput
    periodic
    activeBits
    sparsity
    size
    radius
    resolution

encode :: ScalarEncoder -> Double -> Sdr -> IO ()
encode = scalarEncoder_encode
