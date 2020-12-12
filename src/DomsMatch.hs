{- 
   DomsMatch: code to play a dominoes match between two players.
   
   The top level function is domsMatch - it takes four arguments:
   Two DomsPlayer functions, representing the two players
   n - the number of games to play
   seed - an integer to seed the random number generator
   The function returns a pair showing how many games were won by each player.

   The functions of type DomsPlayer must take four arguments:
   The current Hand
   The current DominoBoard
   The Player (which will be one of P1 or P2)
   The current Scores
   The function returns a tuple containing the Domino to play and End to play it on.

   Further details about types used are available in the assignment specification.

   Code by Emma Norling (September 2020), based upon previous work by Phil Green.

   To use this module, use the line "import DomsMatch" at the top of your code,
   along with any other imports needed for your functions.
 -}
module DomsMatch where
    import System.Random
    import Data.List
    import Debug.Trace

    -- types used in this module
    type Domino = (Int, Int)
    data DominoBoard = InitBoard | Board Domino Domino History deriving (Eq, Show)
    type History = [(Domino, Player, MoveNum)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    type DomsPlayer = Hand -> DominoBoard -> Player -> Scores -> (Domino, End)

    num_in_hand = 9

    {- domsMatch: play a match of n games between two players,
        given a seed for the random number generator
       input: functions to determine the next move for each of the players,
              n (number of games), seed
       output: a pair of integers, indicating the number of games won by each player
     -}

    domsMatch :: DomsPlayer -> DomsPlayer -> Int -> Int -> (Int, Int)
    domsMatch p1 p2 n seed
        = domsGames p1 p2 n (mkStdGen seed) (0, 0)
          where
          domsGames p1 p2 0 gen wins = wins
          domsGames p1 p2 n gen (p1_wins, p2_wins)
            | winner == P1 = domsGames p1 p2 (n-1) gen2 (p1_wins+1, p2_wins)
            | otherwise    = domsGames p1 p2 (n-1) gen2 (p1_wins, p2_wins+1)
              where
              winner = playGame p1 p2 (if odd n then P1 else P2) gen1
              (gen1, gen2) = split gen

    {- playGame: play a single game (where winner is determined by a player reaching
          61 exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' _ _ _ _ (61, _) = P1
          playGame' _ _ _ _ (_, 61) = P2
          playGame' p1 p2 firstPlayer gen scores
            = let
              newScores = playDomsRound p1 p2 firstPlayer currentG scores
              (currentG, nextG) = split gen
              in
              playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

              
    {- shuffleDoms: returns a shuffled set of dominoes, given the number generator -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen
        = [x | (x,y) <- sortBy cmp (zip domSet (randoms gen :: [Int]))]
          where cmp (_,y1) (_,y2) = compare y1 y2

    {- playDomsRound: given two dominoes players, the player to go first, the score at the start of the round,
       and the random number generator, returns the score at the end of the round -}
    playDomsRound :: DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitBoard, scores)
          where
          {- shuffle the dominoes and generate the initial hands -}
          shuffled = shuffleDoms gen
          hand1 = take num_in_hand shuffled
          hand2 = take num_in_hand (drop num_in_hand shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked
             Credit to Brooks Rady to identifying the problem causing the unfair advantage to
             player1, and proposing a solution -}
          playDomsRound' _ _ _ (_, _, _, scores@(61,_)) = scores
          playDomsRound' _ _ _ (_, _, _, scores@(_,61)) = scores
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | p1_blocked && p2_blocked = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               = playDomsRound' p1 p2 P2 newGameState
            | otherwise                = playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end) 
                   | turn == P1 = p1 hand1 board turn (score1, score2)
                   | turn == P2 = p2 hand2 board turn (score1, score2)
              {- playDom will always be successful, because it is only called if the player is not blocked -}
              Just newBoard = playDom turn domino board end
              score = scoreBoard newBoard
              newGameState | turn == P1 = (hand1\\[domino], hand2, newBoard, (if s1 > 61 then score1 else s1, score2))
                           | turn == P2 = (hand1, hand2\\[domino], newBoard, (score1, if s2 > 61 then score2 else s2))
              s1 = score1 + score
              s2 = score2 + score

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- canPlay checks if a particular domino can be played at a particular end of the board -}
    canPlay :: Domino -> End -> DominoBoard -> Bool
    -- if nothing has been played yet, any domino can be played
    canPlay domino end InitBoard = True
    canPlay (m,n) L (Board (val,_) _ _) = m == val || n == val
    canPlay (m,n) R (Board _ (_,val) _) = m == val || n == val

    {- blocked goes through the dominoes in a hand and returns False as soon as it finds one
       that can be played -}
    blocked :: Hand -> DominoBoard -> Bool
    blocked [] _ = True
    blocked (d:ds) board
        = not (canPlay d L board) && not (canPlay d R board) && blocked ds board

    {- played check if a domino has been played. Since dominos can be played in either orientation,
       it needs to check both -}
    played :: Domino -> DominoBoard -> Bool
    played domino InitBoard = False
    played (m,n) (Board _ _ history) = not (null [ d | (d,_,_) <- history, d == (m,n) || d == (n,m) ])

    {- possPlays returns all the valid plays given the current hand and board -}
    possPlays :: Hand -> DominoBoard -> ([Domino],[Domino])
    possPlays hand InitBoard = (hand, hand)
    possPlays hand board
        = ([ d | d <- hand, canPlay d L board ],[ d | d <- hand, canPlay d R board ])

    {- playDom attempts to place a domino at the given end of a board it returns the Just the new board
       if it is a valid play, Nothing otherwise -}
    playDom :: Player -> Domino -> DominoBoard -> End -> Maybe DominoBoard
    playDom player domino InitBoard end = Just (Board domino domino [(domino, player, 1)])
    playDom player (m,n) (Board (val,_) right history) L
        | n == val  = Just (Board (m,n) right (((m,n),player,length history + 1):history)) -- play domino as is
        | m == val  = Just (Board (n,m) right (((n,m),player,length history + 1):history)) -- need to flip domino to play it
        | otherwise = Nothing            -- cannot play domino
    playDom player (m,n) (Board left (_,val) history) R
        | m == val  = Just (Board left (m,n) (history ++ [((m,n), player, length history + 1)])) -- can play domino as is
        | n == val  = Just (Board left (n,m) (history ++ [((n,m), player, length history + 1)])) -- need to flip domino to play it
        | otherwise = Nothing            -- cannot play this domino
     
    {- score takes the number of pips on either end of the board and
       calculates the score in the 3s and 5s scoring method -}
    scoreBoard :: DominoBoard -> Int
    scoreBoard InitBoard = 0
    scoreBoard (Board (l1,l2) (r1,r2) _)
        | l1 == r1 && l2 == r2 = score (l1+l2)  -- case for just one domino played
        | otherwise     = score (left + right)
          where
          left = if l1 == l2 then 2*l1 else l1
          right = if r1 == r2 then 2*r1 else r2
          score n
                | n == 3    = 1 -- 1 "3"
                | n == 5    = 1 -- 1 "5"
                | n == 6    = 2 -- 2 "3"s
                | n == 9    = 3 -- 2 "3"s
                | n == 10   = 2 -- 2 "5"s
                | n == 12   = 4 -- 4 "3"s
                | n == 15   = 8 -- 5 "3"s + 3 "5"s
                | n == 18   = 6 -- 6 "3"s
                | n == 20   = 4 -- 4 "5"s
                | otherwise = 0 -- not a multiple of 3 or 5

    {- scoreN takes a board and a target score and returns each unplayed domino and the end to play it on
       to get that target score -}
    scoreN :: DominoBoard -> Int ->[(Domino, End)]
    scoreN board n = scoreN' board domSet []
                     where
                     scoreN' board [] options = options
                     scoreN' board (domino:rest) options
                        | played domino board = scoreN' board rest options -- skip this domino if already played
                        | otherwise = scoreN' board rest newOptions
                          where
                          leftBoard = playDom P1 domino board L    -- try playing it on the left end
                          rightBoard = playDom P1 domino board R   -- try playing it on the right end
                          -- now for each try, see 1) if it was legal and 2) if it achieved the desired score
                          goodLeft = leftBoard /= Nothing && scoreB leftBoard == n 
                          goodRight = rightBoard /= Nothing && scoreB rightBoard == n
                          -- scoreB is only going to be used if it is Just something, so grab the "something"
                          scoreB (Just board) = scoreBoard board
                          newOptions
                            | goodLeft && goodRight = (domino,L):(domino,R):options -- play either end for this score
                            | goodLeft = (domino,L):options -- left end only for this score
                            | goodRight = (domino,R):options -- right end only for this score
                            | otherwise = options   -- can't achieve this score with this domino

    {- simple dominoes player - simply finds the first legal domino in hand that can be played
       and returns it (with the appropriate end) -}
    randomPlayer :: DomsPlayer
    randomPlayer (domino:rest) board player scores
        | leftBoard /= Nothing = (domino,L)
        | rightBoard /= Nothing = (domino,R)
        | otherwise = randomPlayer rest board player scores
        where
        leftBoard = playDom player domino board L    -- try playing it on the left end
        rightBoard = playDom player domino board R   -- try playing it on the right end
