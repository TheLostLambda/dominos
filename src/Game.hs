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

import Data.List (sort)
import Lib
import System.Random (RandomGen, mkStdGen, randoms)

{- Data Types ----------------------------------------------------------------}

type DomsPlayer = Board -> Hand -> (End, Domino)

{- Public API Functions ------------------------------------------------------}

simplePlayer :: DomsPlayer
simplePlayer board hand = head [(e, d) | d <- hand, e <- [L, R], canPlay board e d]

hsdPlayer :: DomsPlayer
hsdPlayer = undefined

shuffleDoms :: RandomGen g => g -> [Domino]
shuffleDoms rng = map snd . sort $ zip (randoms rng :: [Int]) dominos

playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
playDomsRound p1 p2 seed = undefined

seed = 42
playTurn b p = uncurry (playDom b) . p b -- Needs to return new board and hand
(h1, h2) = splitAt 7 . take 14 . shuffleDoms $ mkStdGen seed