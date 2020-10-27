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

-- Only two ends in our implementation
data End = Left | Right


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
playDom board end domino@[a,b]
  | canPlay board end domino = Just $ atEnd play board end
  | otherwise = Nothing
  where play e = if a == b then a + b else sum $ filter (/=e) domino
          
-- FIXME: Boilerplate for days...
-- Run a function on the given end of a board
atEnd :: (Int -> Int) -> Board -> End -> Board
atEnd f (Board l p r) Left = Board (f l) p r
atEnd f (Board l p r) Right = Board l p (f r)

-- Get the given end of a board
getEnd :: Board -> End -> Int
getEnd (Board l _ _) Left  = l
getEnd (Board _ _ r) Right = r
