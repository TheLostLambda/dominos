{-
  Dominos Programming Assignment for COM2018
  Brooks Rady, October 2020
-}

-- Simple aliases
type Domino = [Int]
type Hand = [Domino]
type Board = [Domino]

-- Only two ends in our implementation
data End = L | R

-- Generate a list of all possible `Domino`s
dominos :: [Domino]
dominos = [[a,b] | a <- [0..6], b <- [a..6]]

-- Has a given domino already been played?
played :: Board -> Domino -> Bool
played = flip elem

-- Can a given domino be played at a given end of the board?
canPlay :: Board -> End -> Domino -> Bool
canPlay board end = any (== getEnd end board)

-- Return a tuple of all possible plays on either end of the board
possPlays :: Board -> Hand -> ([Domino], [Domino])
possPlays board hand = (plays L, plays R)
  where plays end = [d | d <- hand, canPlay board end d]

-- Is the player 'knocking' (nothing can be played)?
blocked :: Board -> Hand -> Bool
blocked board hand = null $ l ++ r
  where (l, r) = possPlays board hand

-- Attempt to play the given domino at one end of a board
playDom :: Board -> End -> Domino -> Maybe Board
playDom board end [a,b]
  | canPlay board end [a,b] = Just $ play end
  | otherwise = Nothing
  where play L = reverse newEnd : board
        play R = board ++ [newEnd]
        newEnd = if a == getEnd end board then [a,b] else [b,a]

-- Get the given `End` of the `Board`
getEnd :: End -> Board -> Int
getEnd L = head . head
getEnd R = last . last

