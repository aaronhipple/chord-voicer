{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Scale
  ( Scale(..)
  , scales
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Note                           ( Note )

data Scale
  = Major
  | NaturalMinor
  | HarmonicMinor
  | JazzMinor
  | WholeTone
  | Diminished
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

scales :: HashMap Scale ([Note] -> [Note])
scales = HashMap.fromList
  [ (Major        , pick [0, 2, 4, 5, 7, 9, 11])
  , (NaturalMinor , pick [0, 2, 3, 5, 7, 8, 10])
  , (HarmonicMinor, pick [0, 2, 3, 5, 7, 8, 11])
  , (JazzMinor    , pick [0, 2, 3, 5, 7, 9, 11])
  , (WholeTone    , pick [0, 2, 4, 6, 8, 10])
  , (Diminished   , pick [0, 1, 3, 4, 6, 7, 9, 10])
  ]
  where pick is ns = (ns !!) <$> is
