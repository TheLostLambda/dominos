{-
  Dominos Programming Assignment for COM2018
  Brooks Rady, October 2020
-}

-- Hide the `Left` and `Right` data constructors from `Either`
import Prelude hiding (Left, Right)

-- Simple aliases
type Domino = [Int] -- FIXME: (Int, Int)?
type Hand = [Domino]

-- A `Board` has two open ends and a list of the `Domino`s that have been played
data Board = Board Int [Domino] Int
  deriving Show

-- Only two ends in our implementation
data End = Left | Right
  deriving Show

-- Generate a list of all possible `Domino`s
dominos :: [Domino]
dominos = [[a,b] | a <- [0..6], b <- [a..6]]


-- Can a given domino be played at a given end of the board?
canPlay :: Board -> End -> Domino -> Bool
canPlay board end = any (== getEnd board end)

-- Is the player 'knocking' (nothing can be played)?
blocked :: Board -> Hand -> Bool
blocked board hand = not $ or [canPlay board e d | e <- [Left, Right], d <- hand]

-- Has a given domino already been played?
played :: Board -> Domino -> Bool
played (Board _ played _) = (`elem` played)

-- Attempt to play the given domino at one end of a board
playDom :: Board -> End -> Domino -> Maybe Board
playDom board@(Board l p r) end dom@[a,b]
  | canPlay board end dom = Just $ play end
  | otherwise = Nothing
  where play Left  = Board newEnd (dom:p) r
        play Right = Board l (dom:p) newEnd
        newEnd = if a == b then a + b else sum $ filter (/= getEnd board end) dom

-- Get the given `End` of the `Board`, adjusted for doubles
getEnd :: Board -> End -> Int
getEnd (Board l _ r) end = if pips > 6 then pips `div` 2 else pips
  where pips = case end of { Left -> l; Right -> r }
