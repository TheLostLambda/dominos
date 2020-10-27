{-
  Dominos Programming Assignment for COM2018
  Brooks Rady, October 2020
-}

-- Simple aliases
type Domino = [Int]  -- FIXME: Tuple maybe?, Implement Eq?
type Hand = [Domino]
type Board = [Domino]

-- Only two ends in our implementation
data End = L | R

-- Generate a list of all possible `Domino`s
dominos :: [Domino]
dominos = [[f,s] | f <- [0..6], s <- [f..6]]

-- Has a given domino already been played?
played :: Board -> Domino -> Bool
played board [f,s] = any (`elem` board) [[f,s], [s,f]]

-- FIXME: Define a `sameDom` function?

-- Can a given domino be played at a given end of the board?
canPlay :: Board -> End -> Domino -> Bool
canPlay board end = any (== getEnd end board)

-- Return a tuple of all possible plays on either end of the board
possPlays :: Board -> Hand -> ([Domino], [Domino])
possPlays board hand = (plays L, plays R)
  where plays end = [d | d <- hand, canPlay board end d]

-- Is the player 'knocking' (nothing can be played)?
blocked :: Board -> Hand -> Bool
blocked board hand = null l && null r
  where (l, r) = possPlays board hand

-- Attempt to play the given domino at one end of a board
playDom :: Board -> End -> Domino -> Maybe Board
playDom board end [f,s]
  | canPlay board end [f,s] = Just $ play end
  | otherwise = Nothing
  where play L = reverse newEnd : board
        play R = board ++ [newEnd]
        newEnd = if f == getEnd end board then [f,s] else [s,f]

scoreBoard :: Board -> Int
scoreBoard board = factorCount 5 + factorCount 3
   where total = pips board L + pips board R
         factorCount fact
           | total `rem` fact == 0 = total `div` fact
           | otherwise = 0

pips board L = if f == s then f + s else f
  where [f,s] = head board
pips board R = if f == s then f + s else s
  where [f,s] = last board

-- FIXME: Maybe don't track the orientation, and just check the neighbour and
-- determine which value they *don't* have in common. That's the one on the
-- outside.

-- Get the given `End` of the `Board`
getEnd :: End -> Board -> Int
getEnd L = head . head
getEnd R = last . last

