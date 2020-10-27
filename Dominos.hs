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
data Board = Board { leftEnd :: Int, playedDoms :: [Domino], rightEnd :: Int }

-- Only two `End`s in our implementation
data End = Left | Right

-- Get the given `End` of a `Board`
getEnd :: End -> Board -> Int
getEnd Left = leftEnd
getEnd Right = rightEnd

-- Can a given `Domino` be played at the given `End` of the `Board`?
canPlay :: Board -> End -> Domino -> Bool
canPlay board end = any (== getEnd board end)

-- Is the player 'knocking' (nothing can be played)?
blocked :: Board -> Hand -> Bool
blocked board hand = not $ or [canPlay board e d | e <- [Left, Right], d <- hand]

-- Has a given `Domino` already been played?
played :: Board -> Domino -> Bool
played board = (`elem` playedDoms board)

-- Attempt to play the given domino at one end of a board
playDom :: Board -> End -> Domino -> Maybe Board
playDom board end domino@[a,b]
  | canPlay board end domino = Just $ board {}
  | otherwise = Nothing
  where play = if a == b then a + b else sum $ filter (/=e) domino
