{-
  AI players for Fives-and-Threes Domino games
  Dominos Programming Assignment for COM2018
  Brooks Rady, December 2020
-}

module AI where -- FIXME: Might rename this module

import Data.List (maximumBy)
import DomsMatch

data GameState = GS Hand DominoBoard Player Scores

type Points = Int
type Play = (Domino, End)
type Tactic = GameState -> [(Points, Play)] -> [(Points, Play)]

-- FIXME: Lots, and maybe sortOn?
strategy :: [Tactic] -> DomsPlayer
strategy tactics hand board player score = snd . maximumBy (\(p, _) -> compare p . fst) $ foldl (\acc t -> t (GS hand board player score) acc) [] tactics

-- FIXME: Could use some spice
byScore :: Tactic
byScore gs _ = map (\p -> (scorePlay gs p, p)) $ allPlays gs

-- FIXME: Spice this up with the applicative of functions? Curry?
allPlays :: GameState -> [Play]
allPlays (GS hand board _ _) = zip l (repeat L) ++ zip r (repeat R)
 where
  (l, r) = possPlays hand board

scorePlay :: GameState -> Play -> Points
scorePlay (GS _ board _ _) (domino, end) = scoreBoard possBoard
 where
  Just possBoard = playDom P1 domino board end

highestPlayer :: DomsPlayer
highestPlayer = strategy [byScore]