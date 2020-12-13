{-
  AI players for Fives-and-Threes Domino games
  Dominos Programming Assignment for COM2018
  Brooks Rady, December 2020
-}

module AI where -- FIXME: Might rename this module

-- Thick Mark: foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFED playerHFE 625) [1..16]

-- FIXME: More list comps, less map and filter

import Data.List
import Data.Ord
import Debug.Trace
import DomsMatch
import Data.Tuple

{- Data Types ----------------------------------------------------------------}

data GameState = GS Hand DominoBoard Player Scores -- FIXME: Make me a record type?
type Play = (Domino, End)
type Points = Int
type Pip = Int
type Tactic = GameState -> [Play] -> [(Play, Points)]

{- Public AI Players ---------------------------------------------------------}

playerH :: DomsPlayer
playerH = strategy [highScoring]

playerHF :: DomsPlayer
playerHF = strategy [highScoring, firstDrop]

playerHFE :: DomsPlayer
playerHFE = strategy [highScoring, firstDrop, endGame]

playerHFEB :: DomsPlayer
playerHFEB = strategy [highScoring, firstDrop, endGame, blindDanger]

playerHFES :: DomsPlayer
playerHFES = strategy [highScoring, firstDrop, endGame, smartDanger]

playerHFEBM :: DomsPlayer
playerHFEBM = strategy [highScoring, firstDrop, endGame, blindDanger, mostPips]

playerHFEBMS :: DomsPlayer
playerHFEBMS = strategy [highScoring, firstDrop, endGame, blindDanger, mostPips, smartDanger]

playerHFEBMS' :: DomsPlayer
playerHFEBMS' = strategy [highScoring, firstDrop, endGame, blindDanger, mostPips, smartDanger']

playerHFEBMS'' :: DomsPlayer
playerHFEBMS'' = strategy [highScoring, firstDrop, endGame, blindDanger, mostPips, smartDanger'']

{- AI Player Tactics ---------------------------------------------------------}

-- FIXME: Could use some spice
highScoring :: Tactic
highScoring (GS _ board _ _) = map (\p -> (p, scorePlay board p))

firstDrop :: Tactic
firstDrop (GS _ InitBoard _ _) plays
  | ((5, 4), L) `elem` plays = [(((5, 4), L), 2)]
  | otherwise = []
firstDrop _ _ = []

endGame :: Tactic
endGame gs@(GS _ board _ _) plays
  | playerScore gs > 50 = zip (getTo 61) (repeat 20) ++ zip (getTo 59) (repeat 10)
  | otherwise = []
 where
  getTo score = intersect plays $ scoreN board (score - playerScore gs)

blindDanger :: Tactic
blindDanger (GS hand _ _ _) plays = map (\p -> (p, -2)) dangerPlays
 where
  dangerPlays = filter (\(d@(n, _), _) -> d `elem` dangerDoms && n `notElem` knockable) plays
  knockable = concatMap (\(a, b) -> [a, b]) $ hand \\ dangerDoms
  dangerDoms = [(6, 6), (5, 5)]

mostPips :: Tactic -- 1 is better than c - 4, empirically
mostPips (GS hand board _ _) plays = [(pl, 1) | (c, p) <- countPips hand, c > 4, pl@(d, _) <- plays, exposesPip pl p]
 where
  exposesPip play@(d, _) pip
    | board == InitBoard = d `hasPip` pip
    | otherwise = newEnd board play == pip

smartDanger :: Tactic
smartDanger gs@(GS _ board player _) = map (\p -> (p, -  dangerScore p))
  where
    oHand = otherHand gs
    oHandOdds = fromIntegral (handSize gs $ other player) / fromIntegral (length oHand)
    dangerScore (dom,end) = round $ fromIntegral (sum [scorePlay newBoard p | p <- allPlays newBoard oHand]) * oHandOdds
      where
        Just newBoard = playDom player dom board end

smartDanger' :: Tactic
smartDanger' gs@(GS _ board player _) = map (\p -> (p, -  dangerScore p))
  where
    oHand = otherHand gs
    oHandOdds = fromIntegral (handSize gs $ other player) / fromIntegral (length oHand)
    dangerScore (dom,end) = round $ fromIntegral (sum . map snd $ mergeBothPlays [(p,scorePlay newBoard p) | p <- allPlays newBoard oHand]) * oHandOdds
      where
        Just newBoard = playDom player dom board end

smartDanger'' :: Tactic
smartDanger'' gs@(GS _ board player _) = map (\p -> (p, -  dangerScore p))
  where
    oHand = otherHand gs
    oHandOdds = fromIntegral (handSize gs $ other player) / fromIntegral (length oHand)
    dangerScore (dom,end) = round $ (sum $ map (\x-> mean [scorePlay newBoard p | p <- allPlays newBoard [x]]) oHand) * oHandOdds
      where
        Just newBoard = playDom player dom board end

debug :: Tactic
debug gs@(GS hand (Board _ _ hist) pl _) _ = trace (show pl ++ " Hand: " ++ show hand ++ "\nHist:" ++ show hist ++ "\nKnocking:" ++ show (knockingPips gs) ++ "\nOther?: " ++ show (otherHand gs) ++ "\nHave: " ++ show (handSize gs $ other pl) ++ "\n\n") []
debug _ _ = []

{- Private Helper Functions --------------------------------------------------}

-- FIXME: Lots, and maybe sortOn?
strategy :: [Tactic] -> DomsPlayer
strategy tactics hand board player score = fst . maximumBy (comparing snd) $ foldl' applyTactic initPlays tactics
 where
  gs = GS hand board player score
  initPlays = map (\p -> (p, 0)) $ allPlays board hand
  applyTactic plays tactic = mergePlays plays $ tactic gs (map fst plays)

mergePlays :: [(Play, Points)] -> [(Play, Points)] -> [(Play, Points)]
mergePlays = foldl' insertPlay
 where
  insertPlay plays new@(d, p) =
    maybe new (\x -> (d, p + x)) (lookup d plays) : filter (\(x, _) -> x /= d) plays

mean :: (Integral a, Fractional b) => [a] -> b
mean [] = 0.0
mean lst = fromIntegral (sum lst) / fromIntegral (length lst)

-- mergeBothPlays lst = map (\((d,e),_) -> ((d,e),sum [p | ((x,_),p) <- lst, d == x])) lst
mergeBothPlays :: [(Play, Points)] -> [(Play, Points)]
mergeBothPlays [] = []
mergeBothPlays lst@(((d,e),s):_) = ((d,e),sum copies `div` length copies) : mergeBothPlays (filter (\((x,_),_) -> x /= d) lst)
  where copies = [p | ((x,_),p) <- lst, d == x]

-- FIXME: Spice this up with the applicative of functions? Curry?
allPlays :: DominoBoard -> Hand -> [Play]
allPlays board hand = zip l (repeat L) ++ zip r (repeat R)
 where
  (l, r) = possPlays hand board

scorePlay :: DominoBoard -> Play -> Points
scorePlay board (domino, end) = scoreBoard possBoard
 where
  Just possBoard = playDom P1 domino board end

playerScore :: GameState -> Points
playerScore (GS _ _ P1 (p, _)) = p
playerScore (GS _ _ P2 (_, p)) = p

-- FIXME: Bit repetitive here, innit?
newEnd :: DominoBoard -> Play -> Pip
newEnd (Board (l, _) _ _) ((f, s), L) = if f == l then s else f
newEnd (Board _ (_, r) _) ((f, s), R) = if f == r then s else f

hasPip :: Domino -> Pip -> Bool
hasPip (f, s) p = p == f || p == s

countPips :: [Domino] -> [(Int, Pip)]
countPips = map (\x -> (length x, head x)) . group . sort . concatMap (\(f, s) -> [f, s])

playedDoms :: History -> [Domino]
playedDoms = concatMap (\(d, _, _) -> [d, swap d])

-- FIXME: Pointfree with a lambda?
endPips :: History -> [Pip]
endPips history = [fst $ head board, snd $ last board]
  where
    board = playedDoms history

other :: Player -> Player
other P1 = P2
other P2 = P1

handSize :: GameState -> Player -> Int
handSize (GS _ InitBoard _ _) _ = num_in_hand
handSize (GS _ (Board _ _ hist) _ _) player = num_in_hand - length [p | (_, p, _) <- hist, p == player]

-- FIXME: Yuck
knockingPips :: GameState -> [Pip]
knockingPips (GS _ (Board _ _ hist) player _) = go hist player
 where
  go [] _ = []
  go hist pl
    | pl == player && pl == p = go (delete last hist) p `union` endPips hist
    | otherwise = go (delete last hist) p
   where
    last@(_, p, _) = maximumBy (comparing $ \(_, _, x) -> x) hist

otherHand :: GameState -> [Domino]
otherHand (GS hand InitBoard _ _) = domSet \\ hand
otherHand gs@(GS hand (Board _ _ hist) _ _) = filter (\d -> not $ any (d `hasPip`) $ knockingPips gs) unknownDoms
  where unknownDoms = domSet \\ (hand ++ playedDoms hist)