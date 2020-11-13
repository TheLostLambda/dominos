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
import System.Random (RandomGen, mkStdGen, randomR)

{- Data Types ----------------------------------------------------------------}

type DomsPlayer = Board -> Hand -> (Domino, End)

{- Public API Functions ------------------------------------------------------}

simplePlayer :: DomsPlayer
simplePlayer = undefined

hsdPlayer :: DomsPlayer
hsdPlayer = undefined

shuffleDoms :: RandomGen g => g -> [Domino]
shuffleDoms = shuffle dominos
  where
    shuffle [] _ = []
    shuffle doms rng = d : shuffle (hs ++ ts) nrng
      where
        (i, nrng) = randomR (0, length doms - 1) rng
        (hs, d : ts) = splitAt i doms

playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
playDomsRound = undefined