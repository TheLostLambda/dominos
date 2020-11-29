{-
  Functions and AI players for Fives-and-Threes Domino games
  Dominos Programming Assignment for COM2018
  Brooks Rady, October 2020
-}

module Game (
    simplePlayer,
    hsdPlayer,
    shuffleDoms,
    playDomsRound,
) where

import Data.List (delete, sort)
import Lib
import System.Random (RandomGen, mkStdGen, randoms)

{- Data Types ----------------------------------------------------------------}

type DomsPlayer = Board -> Hand -> (Domino, End)

{- Public API Functions ------------------------------------------------------}

simplePlayer :: DomsPlayer
simplePlayer board hand = head [(d, e) | d <- hand, e <- [L, R], canPlay board e d]

hsdPlayer :: DomsPlayer
hsdPlayer = undefined

shuffleDoms :: RandomGen g => g -> [Domino]
shuffleDoms rng = map snd . sort $ zip (randoms rng :: [Int]) dominos

-- FIXME: God should smite me for this...
playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
playDomsRound p1 p2 seed = snd $ foldl nextTurn (([], h1, h2), (0, 0)) turns
  where
    turns = take 14 $ cycle [p1, p2]
    (h1, h2) = splitAt 7 . take 14 . shuffleDoms $ mkStdGen seed
    nextTurn ((b, h1, h2), (s1, s2)) p = ((board, h2, hand), (s2, score))
      where
        score = if null h1 || null h2 then s1 else s1 + points
        (board, hand, points) = playTurn p b h1

{- Private Helper Functions --------------------------------------------------}

playTurn :: DomsPlayer -> Board -> Hand -> (Board, Hand, Int)
playTurn p b h
    | blocked b h = (b, h, 0)
    | otherwise = (board, delete d h, scoreBoard board)
  where
    Just board = playDom b e d
    (d, e) = p b h