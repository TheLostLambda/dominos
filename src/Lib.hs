{-
  Basic types and functions for modelling Dominos
  Dominos Programming Assignment for COM2018
  Brooks Rady, October 2020
-}

module Lib (
    Domino,
    Hand,
    Board,
    End (L, R),
    dominos,
    canPlay,
    blocked,
    played,
    possPlays,
    playDom,
    scoreBoard,
    scoreN,
) where

import Data.Tuple (swap)

{- Data Types ----------------------------------------------------------------}

type Domino = (Int, Int) -- A pair of pip counts (0-6)
type Hand = [Domino] -- An unordered list of `Domino`s
type Board = [Domino] -- Head is the left of the board, tail is the right
data End = L | R deriving (Eq) -- The left and right sides of a Board

{- Public API Functions ------------------------------------------------------}

-- Generate a list of all possible `Domino`s
dominos :: [Domino]
dominos = [(f, s) | f <- [0 .. 6], s <- [f .. 6]]

-- Has a given domino already been played?
played :: Board -> Domino -> Bool
played board domino = any (sameDom domino) board

-- Can a given domino be played at a given end of the board?
canPlay :: Board -> End -> Domino -> Bool
canPlay board end (f, s) = null board || f == e || s == e
  where
    e = getEnd end board

-- Return a tuple of all possible plays on either end of the board
possPlays :: Board -> Hand -> ([Domino], [Domino])
possPlays board hand = (plays L, plays R)
  where
    plays end = [d | d <- hand, canPlay board end d]

-- Is the player 'knocking' (nothing can be played)?
blocked :: Board -> Hand -> Bool
blocked board hand = null l && null r
  where
    (l, r) = possPlays board hand

-- Attempt to play the given domino at one end of a board
playDom :: Board -> End -> Domino -> Maybe Board
playDom [] _ domino = Just [domino]
playDom board end (f, s)
    | canPlay board end (f, s) = Just $ play end
    | otherwise = Nothing
  where
    play L = swap newEnd : board
    play R = board ++ [newEnd]
    newEnd = if f == getEnd end board then (f, s) else (s, f)

-- Perform fives-and-threes scoring by counting the number of factors
scoreBoard :: Board -> Int
scoreBoard [(f, s)] = factorCount 5 (f + s) + factorCount 3 (f + s)
scoreBoard board = factorCount 5 total + factorCount 3 total
  where
    total = pips board L + pips board R

-- Return all possible plays with a score of N
scoreN :: Board -> Int -> ([Domino], [Domino])
scoreN board score = (scoredPlays L, scoredPlays R)
  where
    scoredPlays end = [d | (Just b, d) <- allPlays end, scoreBoard b == score]
    allPlays end = [(playDom board end d, d) | d <- dominos, not $ played board d]

{- Private Helper Functions --------------------------------------------------}

-- Determine if two differently oriented `Domino`s are really the same
sameDom :: Domino -> Domino -> Bool
sameDom x y = x `elem` [y, swap y]

-- Get the given `End` of the `Board`
getEnd :: End -> Board -> Int
getEnd L = fst . head
getEnd R = snd . last

-- Count the number of scorable pips on a given end of the board
pips :: Board -> End -> Int
pips board end = if f == s then f + s else f
  where
    (f, s) = case end of
        L -> head board
        R -> swap $ last board

factorCount :: Int -> Int -> Int
factorCount fact total
    | total `rem` fact == 0 = total `div` fact
    | otherwise = 0