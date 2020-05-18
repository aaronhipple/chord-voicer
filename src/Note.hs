{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Utilities for working with notes.

module Note
  ( Note(..)
  , from
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )

data Note
  = N1  -- C
  | N2  -- Db
  | N3  -- D
  | N4  -- Eb
  | N5  -- E
  | N6  -- F
  | N7  -- Gb
  | N8  -- G
  | N9  -- Ab
  | N10 -- A
  | N11 -- Bb
  | N12 -- B
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving anyclass (Hashable)

from :: String -> Note
from "C"  = N1
from "C#" = N2
from "Db" = N2
from "D"  = N3
from "D#" = N4
from "Eb" = N4
from "E"  = N5
from "F"  = N6
from "F#" = N7
from "Gb" = N7
from "G"  = N8
from "G#" = N9
from "Ab" = N9
from "A"  = N10
from "A#" = N11
from "Bb" = N11
from "B"  = N12
from n    = error $ "Unknown note name: " ++ n
