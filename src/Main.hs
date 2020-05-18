{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import           GHC.Generics                   ( Generic )
import qualified Data.HashSet                  as HashSet
import           Data.HashSet                   ( HashSet )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Hashable                  ( Hashable )
import           Control.Monad                  ( guard
                                                , forM_
                                                )
import qualified Data.List                     as List
import           Data.Maybe                     ( catMaybes )

import           System.Environment             ( getArgs )
import           Note                           ( Note(..) )
import qualified Note


main :: IO ()
main = do
  notes <- getArgs
  let vs = voicings $ layout guitar $ Note.from <$> notes
  forM_ (take 10 $ vs) print

maxStretch :: Int
maxStretch = 4

guitar :: [[Note]]
guitar = map (course' 13 . Note.from) ["E", "A", "D", "G", "B", "E"]
 where
  course' :: Int -> Note -> [Note]
  course' frets note = take frets $ cycle notes
   where
    notes | note == minBound = [note ..]
          | otherwise        = [note ..] ++ [N1 .. pred note]

newtype Layout = Layout [[Bool]]

guideLine :: String
guideLine = "\\\\          '       '       '       '           \""

instance Show Layout where
  show (Layout courses) = unlines $ (showCourse <$> courses) ++ [guideLine]
   where

    showCourse :: [Bool] -> String
    showCourse [] = ""
    showCourse (x : xs) =
      showFirst x ++ "   " ++ (List.intercalate " | " (showRest <$> xs))

    showFirst True  = "o"
    showFirst False = "|"

    showRest True  = "x"
    showRest False = "-"

layout :: [[Note]] -> [Note] -> Layout
layout neck notes = Layout $ (fmap . fmap) (flip HashSet.member $ noteSet) neck
  where noteSet = HashSet.fromList notes

voicings :: Layout -> [Fingering]
voicings (Layout courses) = do
  voicing <- sequence $ List.findIndices id <$> courses
  guard $ doesNotUseMoreFretsThan maxStretch voicing
  guard $ doesNotStretchMoreThan maxStretch voicing
  take 1 $ fingerings $ Just <$> voicing
 where
  doesNotStretchMoreThan n ns
    | all (== 0) ns = True
    | otherwise     = (maximum ns) - (minimum $ nonZero ns) <= n

  doesNotUseMoreFretsThan n ns =
    n >= (HashSet.size $ HashSet.fromList $ nonZero ns)

nonZero :: [Int] -> [Int]
nonZero = filter (/= 0)

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
