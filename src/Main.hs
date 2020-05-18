{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import           GHC.Generics                   ( Generic )
import qualified Data.HashSet                  as HashSet
import           Data.HashSet                   ( HashSet )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict            ( )
import           Data.Hashable                  ( Hashable )
import           Control.Monad                  ( guard
                                                , forM_
                                                )
import qualified Data.List                     as List
import           Data.Maybe                     ( catMaybes )


main :: IO ()
main = do
  putStrLn "hello world"

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

fromName :: String -> Note
fromName "C"  = N1
fromName "C#" = N2
fromName "Db" = N2
fromName "D"  = N3
fromName "D#" = N4
fromName "Eb" = N4
fromName "E"  = N5
fromName "F"  = N6
fromName "F#" = N7
fromName "Gb" = N7
fromName "G"  = N8
fromName "G#" = N9
fromName "Ab" = N9
fromName "A"  = N10
fromName "A#" = N11
fromName "Bb" = N11
fromName "B"  = N12
fromName n    = error $ "Unknown note name: " ++ n

maxStretch :: Int
maxStretch = 4

gv :: [String] -> IO ()
gv notes = do
  let vs = voicings $ layout guitar $ notes
  forM_
    (take 10 $ chunksOf 5 vs)
    (\chunk -> do
      putStr (showByRow $ show <$> chunk)
      putStr "\n"
    )

showByRow :: [String] -> String
showByRow input = unlines $ (List.intercalate "   " <$> List.transpose broken)
  where broken = lines <$> input

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = chunk : chunksOf n rest where (chunk, rest) = splitAt n xs

guitar :: [[Note]]
guitar = map (course' 12 . fromName) ["E", "A", "D", "G", "B", "E"]
 where
  course' :: Int -> Note -> [Note]
  course' frets note = take frets $ cycle notes
   where
    notes | note == minBound = [note ..]
          | otherwise        = [note ..] ++ [N1 .. pred note]

newtype Layout = Layout [[Bool]]

instance Show Layout where
  show (Layout courses) = unlines $ reverse $ fmap showCourse courses
   where
    showCourse (True  : xs) = 'o' : fmap showNote xs
    showCourse (False : xs) = '|' : fmap showNote xs
    showCourse xs           = fmap showNote xs

    showNote False = '-'
    showNote True  = 'x'

layout :: [[Note]] -> [String] -> Layout
layout neck notes = Layout $ (fmap . fmap) (flip HashSet.member $ noteSet) neck
  where noteSet = HashSet.fromList $ fromName <$> notes

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
    unlines $ List.transpose $ (uncurry showCourse) <$> zip [0 ..] placements
   where
    maxIndex    = length placements - 1
    fretsToShow = 12
    showCourse :: Int -> Placement -> String
    showCourse n Skip = (topChar n) : (replicate fretsToShow (lowerChar n))
    showCourse n Open = '○' : (replicate fretsToShow (lowerChar n))
    showCourse n (Fingered frt fng) = concat
      [ [topChar n]
      , replicate (frt - 1) (lowerChar n)
      , [forFinger fng]
      , replicate (fretsToShow - (frt)) (lowerChar n)
      ]

    topChar n | n == maxIndex = '╕'
              | n == 0        = '╒'
              | otherwise     = '╤'
    lowerChar n | n == 0        = '├'
                | n == maxIndex = '┤'
                | otherwise     = '┼'

    forFinger Index  = '➊'
    forFinger Middle = '➋'
    forFinger Ring   = '➌'
    forFinger Little = '➍'



toFinger :: Placement -> Maybe Finger
toFinger Skip     = Nothing
toFinger Open     = Nothing
toFinger fingered = Just $ finger fingered


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
