{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import qualified Data.HashSet                  as HashSet
import           Control.Monad                  ( guard
                                                , forM_
                                                )
import qualified Data.List                     as List

import           System.Environment             ( getArgs )
import           Note                           ( Note(..) )
import qualified Note
import           Fingering                      ( Fingering(..)
                                                , fingerings
                                                )
import           Output                         ( guideLine )


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
