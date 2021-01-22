module Interactive where

import AI
import Data.Maybe
import Data.Tuple (swap)
import DomsMatch
import Data.Char
import Text.Read

human :: DomsPlayer
human hand board player scores = do
    showGameState board player scores
    play <- getPlay hand board
    case validateChoice play hand board of
        Left err -> do
            putStrLn err
            human hand board player scores
        Right play -> return play

showGameState :: DominoBoard -> Player -> Scores -> IO ()
showGameState board player scores = do
    let boardStr = case board of
            InitBoard -> "Empty!"
            Board _ _ hist -> show [d | (d, _, _) <- hist]
        (selfS, otherS) = if player == P1 then scores else swap scores
    putStrLn $ "My Score: " ++ show selfS ++ "; Other Score: " ++ show otherS
    putStrLn $ "Board: " ++ boardStr

getPlay :: Hand -> DominoBoard -> IO (String, String)
getPlay hand board = do
    mapM_ (\(i, dom) -> putStrLn $ show i ++ ": " ++ show dom) $ zip [0 ..] hand
    idx <- getLine
    putStrLn "End?"
    end <- getLine
    return (idx, end)

validateChoice :: (String, String) -> Hand -> DominoBoard -> Either String Play
validateChoice (idxStr, endStr) hand board
    | isNothing idx || isNothing end = Left "Failed to parse"
    | fromJust idx `notElem` [i | (i, _) <- zip [0 ..] hand] = Left "Invalid index"
    | not $ canPlay dom (fromJust end) board = Left "Illegal play"
    | otherwise = Right (dom, fromJust end)
  where
    idx = readMaybe idxStr
    end = readMaybe $ map toUpper endStr
    dom = hand !! fromJust idx

documentPlayer :: DomsPlayer -> DomsPlayer
documentPlayer ai hand board player scores = do
    play@(dom, end) <- ai hand board player scores
    let endStr = if end == L then "left" else "right"
    putStrLn $ "Opponent played " ++ show dom ++ " on the " ++ endStr
    return play