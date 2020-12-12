{-
  AI players for Fives-and-Threes Domino games
  Dominos Programming Assignment for COM2018
  Brooks Rady, December 2020
-}

module AI where -- FIXME: Might rename this module

-- Thick Mark: foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFED playerHFE 625) [1..16]

-- FIXME: More list comps, less map and filter

import Data.List
import Debug.Trace
import DomsMatch

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

playerHFEBM :: DomsPlayer
playerHFEBM = strategy [highScoring, firstDrop, endGame, blindDanger, mostPips]

{- AI Player Tactics ---------------------------------------------------------}

firstDrop :: Tactic
firstDrop (GS _ InitBoard _ _) plays
  | ((5, 4), L) `elem` plays = [(((5, 4), L), 2)]
  | otherwise = []
firstDrop _ _ = []

-- FIXME: Could use some spice
highScoring :: Tactic
highScoring gs = map (\p -> (p, scorePlay gs p))

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

mostPips :: Tactic
mostPips (GS hand board _ _) plays = [(pl, 1) | (c, p) <- countPips hand, c > 4, pl@(d, _) <- plays, exposesPip pl p]
 where
  exposesPip play@(d, _) pip
    | board == InitBoard = d `hasPip` pip
    | otherwise = newEnd board play == pip

{- Private Helper Functions --------------------------------------------------}

-- FIXME: Lots, and maybe sortOn?
strategy :: [Tactic] -> DomsPlayer
strategy tactics hand board player score = fst . maximumBy (onSnd compare) $ foldl' applyTactic initPlays tactics
 where
  gs = GS hand board player score
  initPlays = map (\p -> (p, 0)) $ allPlays gs
  applyTactic plays tactic = mergePlays plays $ tactic gs (map fst plays)

mergePlays :: [(Play, Points)] -> [(Play, Points)] -> [(Play, Points)]
mergePlays = foldl' insertPlay
 where
  insertPlay plays new@(d, p) =
    maybe new (\x -> (d, p + x)) (lookup d plays) : deleteBy (onFst (==)) new plays

-- FIXME: Spice this up with the applicative of functions? Curry?
allPlays :: GameState -> [Play]
allPlays (GS hand board _ _) = zip l (repeat L) ++ zip r (repeat R)
 where
  (l, r) = possPlays hand board

scorePlay :: GameState -> Play -> Points
scorePlay (GS _ board _ _) (domino, end) = scoreBoard possBoard
 where
  Just possBoard = playDom P1 domino board end

playerScore :: GameState -> Points
playerScore (GS _ _ P1 (p, _)) = p
playerScore (GS _ _ P2 (_, p)) = p

onFst :: (a -> a -> c) -> (a, b) -> (a, b) -> c
onFst f (a, _) (b, _) = f a b

onSnd :: (b -> b -> c) -> (a, b) -> (a, b) -> c
onSnd f (_, a) (_, b) = f a b

-- FIXME: Bit repetitive here, innit?
newEnd :: DominoBoard -> Play -> Pip
newEnd (Board (l, _) _ _) ((f, s), L) = if f == l then s else f
newEnd (Board _ (_, r) _) ((f, s), R) = if f == r then s else f

hasPip :: Domino -> Pip -> Bool
hasPip (f, s) p = p == f || p == s

countPips :: [Domino] -> [(Int, Pip)]
countPips = map (\x -> (length x, head x)) . group . sort . concatMap (\(f, s) -> [f, s])