{-
  AI players for Fives-and-Threes Domino games
  Dominos Programming Assignment for COM2018
  Brooks Rady, December 2020
-}

module AI where

import Data.List (delete, foldl', group, intersect, maximumBy, sort, union, (\\))
import Data.Ord (comparing)
import DomsMatch

{- Data Types ------------------------------------------------------------------------------------}

-- Wraps all of the args of `DomsPlayer` into a single, easy-to-pass value
data GameState = GS Hand DominoBoard Player Scores

-- Tactics score a list of plays according to strategic advantage they provide
type Tactic = GameState -> [Play] -> [(Play, Points)]

-- Miscellaneous type synonyms for clarity
type Play = (Domino, End)
type Points = Int
type Pip = Int

{- Public AI Players -----------------------------------------------------------------------------}

-- Simply play the highest scoring domino possible
playerH :: DomsPlayer
playerH = strategy [highScoring]

-- Prioritize (5,4) as the first domino to drop
playerHF :: DomsPlayer
playerHF = strategy [highScoring, firstDrop]

-- Past scores of 50, look for plays that land on 59 (many ways to win) or win outright
playerHFE :: DomsPlayer
playerHFE = strategy [highScoring, firstDrop, endGame]

-- Discourage dropping dangerous dominos – (6,6) and (5,5) – if they can't be knocked off
playerHFEB :: DomsPlayer
playerHFEB = strategy [highScoring, firstDrop, endGame, blindDanger]

-- Encourage the dropping of dominos that expose pips that the player holds the majority of
playerHFEBM :: DomsPlayer
playerHFEBM = strategy [highScoring, firstDrop, endGame, blindDanger, mostPips]

-- Predict the opponent's hand and discourage plays that are likely to yield them points
playerHFEBMS :: DomsPlayer
playerHFEBMS = strategy [highScoring, firstDrop, endGame, blindDanger, mostPips, smartDanger]

{- AI Player Tactics -----------------------------------------------------------------------------}

-- Zip each play with the points it would score
highScoring :: Tactic
highScoring (GS _ board _ _) = map (\p -> (p, scorePlay board p))

-- If the board is empty, encourage dropping (5,4)
-- NB: Adding 2 to the existing (5,4) score of 3, puts it above the score of any other first play
firstDrop :: Tactic
firstDrop (GS _ InitBoard _ _) plays
    | ((5, 4), L) `elem` plays = [(((5, 4), L), 2)] -- If (5,4) is present, L is always possible
    | otherwise = []
firstDrop _ _ = []

-- Check if it's possible to either win the game or get to 59, *strongly* encouraging those plays
endGame :: Tactic
endGame gs@(GS _ board _ _) plays
    | playerScore gs > 50 = zip (getTo 61) (repeat 30) ++ zip (getTo 59) (repeat 15)
    | otherwise = []
  where
    getTo score = intersect plays $ scoreN board (score - playerScore gs)

-- Discourage dropping "dangerous" dominos if they can't be knocked off
-- NB: The dangerous dominos – (6,6) and (5,5) – and the penalty of -2 were determined empirically
blindDanger :: Tactic
blindDanger (GS hand _ _ _) = map (\p -> (p, -2)) . dangerPlays
  where
    -- Find plays with dangerous dominos and unowned pips
    dangerPlays = filter (\(d@(n, _), _) -> d `elem` dangerDoms && n `notElem` ownedPips)
    -- Find all pips in the player's hand (excluding the dangerous dominos themselves)
    ownedPips = concatMap (\(a, b) -> [a, b]) $ hand \\ dangerDoms
    dangerDoms = [(6, 6), (5, 5)]

-- Encourage plays that expose pips the player "controls"
-- NB: "Controlled" pips are those that the player owns the majority of (more than 4)
mostPips :: Tactic
mostPips (GS hand board _ _) plays =
    [(pl, c - 4) | (c, p) <- countPips hand, c > 4, pl <- plays, p `elem` newEnds' board pl]

-- Given what the opponent could be holding, calculate the average risk of each play
-- NB: Risk is a scaled average of all of the points an opponent could score in response to a play
smartDanger :: Tactic
smartDanger gs@(GS _ board player _) = map (\p -> (p, - dangerScore p))
  where
    oHand = otherHand gs -- All of the dominos that an opponent could have
    oHandOdds = otherHandSize gs // length oHand -- How likely they are to hold each of those
    -- Simulate every play and calculate what the opponent could score in response
    dangerScore (domino, end) = round $ sum (map domRisk oHand) * oHandOdds -- Scale by confidence
      where
        -- For every opponent domino, calculate the retaliation score – averaging L and R plays
        domRisk d = mean . map (scorePlay newBoard) $ allPlays newBoard [d]
        -- Peek at a possible future (the play is known to be legal)
        Just newBoard = playDom player domino board end

{- Player Tactic Composition ---------------------------------------------------------------------}

-- FIXME: Lots, and maybe sortOn?
strategy :: [Tactic] -> DomsPlayer
strategy tactics hand board player score =
    fst . maximumBy (comparing snd) $ foldl' applyTactic initPlays tactics
  where
    gs = GS hand board player score
    initPlays = map (\p -> (p, 0)) $ allPlays board hand
    applyTactic plays tactic = mergePlays plays $ tactic gs (map fst plays)

mergePlays :: [(Play, Points)] -> [(Play, Points)] -> [(Play, Points)]
mergePlays = foldl' insertPlay
  where
    insertPlay plays new@(d, p) =
        maybe new (\x -> (d, p + x)) (lookup d plays) : filter (\(x, _) -> x /= d) plays

{- Opponent Prediction ---------------------------------------------------------------------------}

otherHandSize :: GameState -> Int
otherHandSize (GS _ InitBoard _ _) = num_in_hand
otherHandSize (GS _ (Board _ _ history) player _) =
    num_in_hand - length [p | (_, p, _) <- history, p /= player]

-- FIXME: Yuck
otherHand :: GameState -> [Domino]
otherHand (GS hand InitBoard _ _) = domSet \\ hand
otherHand (GS hand board@(Board _ _ history) player _) = filter (not . hasKnockingPip) unknownDoms
  where
    unknownDoms = filter (not . flip played board) $ domSet \\ hand
    hasKnockingPip d = any (d `hasPip`) $ knockingPips history player
    knockingPips [] _ = []
    knockingPips hist pl
        | pl == player && pl == lastPl = knockingPips (delete lastPlay hist) lastPl `union` endPips hist
        | otherwise = knockingPips (delete lastPlay hist) lastPl
      where
        lastPlay@(_, lastPl, _) = maximumBy (comparing $ \(_, _, x) -> x) hist

{- Domino Helper Functions -----------------------------------------------------------------------}

-- Fetch the score of the current player
playerScore :: GameState -> Points
playerScore (GS _ _ P1 (p, _)) = p
playerScore (GS _ _ P2 (_, p)) = p

-- Return a list of all possible plays, given some board and hand
allPlays :: DominoBoard -> Hand -> [Play]
allPlays board hand = zip l (repeat L) ++ zip r (repeat R)
  where
    (l, r) = possPlays hand board

-- Simulate a play and calculate its score
scorePlay :: DominoBoard -> Play -> Points
scorePlay board (domino, end) = scoreBoard possBoard
  where
    Just possBoard = playDom P1 domino board end

-- Simulate a play and return the new ends of the board
newEnds' :: DominoBoard -> Play -> [Pip]
newEnds' board (domino, end) = [l, r]
  where
    Just (Board (l, _) (_, r) _) = playDom P1 domino board end

-- Find the free ends of a domino board at some point in the past
endPips :: History -> [Pip]
endPips history = [fst $ head board, snd $ last board]
  where
    board = [d | (d, _, _) <- history]

-- Check if a domino contains a given pip
hasPip :: Domino -> Pip -> Bool
hasPip (f, s) p = p == f || p == s

-- Convert a list of dominos into a count of their pips
-- The sort-group-map approach is my favorite trick for generating a frequency list ;)
countPips :: [Domino] -> [(Int, Pip)]
countPips = map (\x -> (length x, head x)) . group . sort . concatMap (\(f, s) -> [f, s])

{- General Helper Functions ----------------------------------------------------------------------}

-- Performs the floating-point division of integers
(//) :: (Integral a, Integral b, Fractional c) => a -> b -> c
x // y = fromIntegral x / fromIntegral y

-- Calculates the average value of a list
mean :: (Integral a, Fractional b) => [a] -> b
mean [] = 0.0
mean lst = sum lst // length lst