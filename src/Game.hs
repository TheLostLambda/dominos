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

import Lib
import Data.List (sort)
import System.Random (RandomGen, mkStdGen, randoms)

{- Data Types ----------------------------------------------------------------}

type DomsPlayer = Board -> Hand -> (Domino, End)

{- Public API Functions ------------------------------------------------------}

simplePlayer :: DomsPlayer
simplePlayer = undefined

hsdPlayer :: DomsPlayer
hsdPlayer = undefined

shuffleDoms :: RandomGen g => g -> [Domino]
shuffleDoms rng  = map snd . sort $ zip (randoms rng :: [Int]) dominos

playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
playDomsRound = undefined