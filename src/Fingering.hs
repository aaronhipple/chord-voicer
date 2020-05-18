{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Fingering
  ( Fingering(..)
  , fingerings
  )
where

import           GHC.Generics                   ( Generic )
import qualified Data.HashSet                  as HashSet
import           Data.HashSet                   ( HashSet )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Hashable                  ( Hashable )
import           Control.Monad                  ( guard )
import qualified Data.List                     as List
import           Data.Maybe                     ( catMaybes )
import           Output                         ( guideLine )


-- TODO Thumb is a special case. How to do?
data Finger
  = Index
  | Middle
  | Ring
  | Little
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving anyclass (Hashable)

data Placement
  = Fingered
  { fret :: Int
  , finger :: Finger
  }
  | Open
  | Skip
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

newtype Fingering = Fingering [Placement]

instance Show Fingering where
  show (Fingering placements) =
    unlines $ (showCourse <$> reverse placements) ++ [guideLine]
   where
    fretsToShow = 12

    showCourse :: Placement -> String
    showCourse Skip =
      "|" ++ "   " ++ List.intercalate " | " (replicate fretsToShow "-")
    showCourse Open =
      "o" ++ "   " ++ List.intercalate " | " (replicate fretsToShow "-")
    showCourse (Fingered frt fng) = "|" ++ "   " ++ List.intercalate
      " | "
      (  (replicate (frt - 1) "-")
      ++ [forFinger fng]
      ++ (replicate (fretsToShow - frt) "-")
      )

    forFinger = show . (+ 1) . fromEnum

placement :: Int -> [Finger] -> [Placement]
placement _   []      = []
placement 0   _       = [Open]
placement frt fingers = [ Fingered frt fng | fng <- fingers ]

allFingers :: HashSet Finger
allFingers = HashSet.fromList $ [Index .. Little]

fingerings :: [Maybe Int] -> [Fingering]
fingerings frets = map Fingering $ ps [] frets
 where
  ps :: [Placement] -> [Maybe Int] -> [[Placement]]
  ps _ [] = [[]]
  ps placements (Nothing : nextFrets) = (Skip :) <$> ps placements nextFrets
  ps placements (Just thisFret : nextFrets) = do
    pl <- placement thisFret unusedFingers
    guard $ placementDoesNotCrossFingers pl
    (pl :) <$> ps (pl : placements) nextFrets
   where
    fingerPlacements = foldr load HashMap.empty placements
     where
      load (  Skip          ) hm = hm
      load (  Open          ) hm = hm
      load p@(Fingered _ fng) hm = HashMap.insert fng p hm

    unusedFingers =
      HashSet.toList $ allFingers `HashSet.difference` usedFingers
    usedFingers = HashMap.keysSet fingerPlacements

    placementDoesNotCrossFingers Skip               = True
    placementDoesNotCrossFingers Open               = True
    placementDoesNotCrossFingers (Fingered frt fng) = and $ concat
      [ (>= frt) <$> fret <$> catMaybes
        (flip HashMap.lookup fingerPlacements <$> [fng ..])
      , (<= frt) <$> fret <$> catMaybes
        (flip HashMap.lookup fingerPlacements <$> [minBound .. fng])
      ]
