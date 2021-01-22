module Interactive where

type Human = Bool -- Is it the human's turn / did they win?

playInteractiveGame :: DomsPlayer -> Int -> IO Human
playInteractiveGame bot seed = playGame' (odd seed) (mkStdGen seed) (0, 0)
 where
  playGame' _ _ (61, _) = return True
  playGame' _ _ (_, 61) = return False
  playGame' humanFirst gen scores = do
    let (currentG, nextG) = split gen
    newScores <- playInteractiveRound bot humanFirst currentG scores 
    playGame' bot !humanFirst nextG newScores

playInteractiveRound :: DomsPlayer -> Human -> StdGen -> (Int, Int) -> IO (Int, Int)
playInteractiveRound player humanFirst gen scores = do
  let shuffled = shuffleDoms gen
      hand1 = take num_in_hand shuffled
      hand2 = take num_in_hand (drop num_in_hand shuffled)
  playDomsRound' player humanFirst (hand1, hand2, InitBoard, scores)
 where
  playDomsRound' _ _ _ (_, _, _, scores@(61, _)) = return scores
  playDomsRound' _ _ _ (_, _, _, scores@(_, 61)) = scores
  playDomsRound' player p2 turn gameState@(hand1, hand2, board, (score1, score2))
    | p1_blocked && p2_blocked = do
      putStrLn $ "Round finished with "
      return (score1, score2)
    | turn == P1 && p1_blocked = playDomsRound' player p2 P2 gameState
    | turn == P2 && p2_blocked = playDomsRound' player p2 P1 gameState
    | turn == P1 = playDomsRound' player p2 P2 newGameState
    | otherwise = playDomsRound' player p2 P1 newGameState
   where
    p1_blocked = blocked hand1 board
    p2_blocked = blocked hand2 board
    (domino, end)
      | turn == P1 = player hand1 board turn (score1, score2)
      | turn == P2 = p2 hand2 board turn (score1, score2)
    {- playDom will always be successful, because it is only called if the player is not blocked -}
    Just newBoard = playDom turn domino board end
    score = scoreBoard newBoard
    newGameState
      | turn == P1 = (hand1 \\ [domino], hand2, newBoard, (if s1 > 61 then score1 else s1, score2))
      | turn == P2 = (hand1, hand2 \\ [domino], newBoard, (score1, if s2 > 61 then score2 else s2))
    s1 = score1 + score
    s2 = score2 + score