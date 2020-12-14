{-
  AI players for Fives-and-Threes Domino games
  Dominos Programming Assignment for COM2018

  Players are ordered from least to most intelligent, so testing `playerH` against
  `playerHFEBMS` should yield the biggest difference in performance and make for
  the most interesting matchup.

  Brooks Rady, December 2020
-}

module AI where

import Data.List (delete, group, intersect, maximumBy, sort, union, (\\))
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
highScoring (GS _ board _ _) = zipFn (scorePlay board)

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
blindDanger (GS hand _ _ _) = zipFn (const $ -2) . dangerPlays
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
smartDanger gs@(GS _ board player _) = zipFn (negate . dangerScore)
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

-- The core of every domino player – composing individual tactics into an overall strategy
-- Every play is individually assigned a score by the tactics, then `strategy` then accumulates
-- those scores and returns the highest cumulatively-ranked play
strategy :: [Tactic] -> DomsPlayer
strategy tactics hand board player score =
    fst . maximumBy (comparing snd) $ foldr applyTactic initPlays tactics
  where
    gs = GS hand board player score -- This function also wraps its arguments into a GameState
    initPlays = zipFn (const 0) $ allPlays board hand -- Initially assign all plays a rank of zero
    applyTactic tactic plays = mergePlays plays $ tactic gs (map fst plays)

-- Merges two play lists; if the same play exists in both lists, sum their point score
mergePlays :: [(Play, Points)] -> [(Play, Points)] -> [(Play, Points)]
mergePlays = foldr insertPlay
  where
    insertPlay new@(p, s) plays =
        maybe new (\x -> (p, s + x)) (lookup p plays) : filter (\(x, _) -> x /= p) plays

{- Opponent Prediction ---------------------------------------------------------------------------}

-- Look at the game state and calculate the number of dominos the opponent is holding
otherHandSize :: GameState -> Int
otherHandSize (GS _ InitBoard _ _) = num_in_hand -- Default to the starting hand size
-- Otherwise, subtract the number of dominos that the opponent has played on the board
otherHandSize (GS _ (Board _ _ history) player _) =
    num_in_hand - length [p | (_, p, _) <- history, p /= player]

-- Return a complete list of the dominos that could be in the opponent's hand, narrowed down by
-- what is in the current players hand, what's on the board, and what the opponent has knocked on
otherHand :: GameState -> [Domino]
otherHand (GS hand InitBoard _ _) = domSet \\ hand -- Here, we only know it's not one of ours
otherHand (GS hand board@(Board _ _ history) player _) = filter (not . hasKnockingPip) unknownDoms
  where
    -- Dominos not on the board or in our hand could belong to the opponent
    unknownDoms = filter (not . flip played board) $ domSet \\ hand
    -- Checks if a domino contains a pip that the opponent has previously knocked on
    hasKnockingPip d = any (d `hasPip`) $ knockingPips history player
    -- Works backwards through the play history, looking for where the current player has gone
    -- twice in a row (i.e. where the opponent is knocking), recording the exposed pips knocked on
    knockingPips [] _ = []
    knockingPips hist pl
        -- Check if the current player went the turn before this one, adding knocking pips if so
        | pl == player && pl == lastPl = pastKnocks `union` endPips hist
        | otherwise = pastKnocks
      where
        -- Get the last play by finding the entry with the highest `MoveNum`
        lastPlay@(_, lastPl, _) = maximumBy (comparing $ \(_, _, x) -> x) hist
        -- Step backwards in time and recurse
        pastKnocks = knockingPips (delete lastPlay hist) lastPl

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

-- Apply a function to every item in a list, zipping the original and result values
zipFn :: (a -> b) -> [a] -> [(a, b)]
zipFn f = map (\x -> (x, f x))
