module Main where

import AI
import Control.Parallel.Strategies
import Data.Foldable
import Data.List
import DomsMatch
import System.IO

seed :: Int
seed = 42

games :: Int
games = 10000

tactics :: [(String, [Tactic])]
tactics =
    [ ("H", [highScoring])
    , ("F", [firstDrop])
    , ("E", [endGame])
    , ("B", [blindDanger])
    , ("M", [mostPips])
    , ("S", [smartDanger])
    ]

main :: IO ()
main = do
    let players = [(n, strategy ts) | (n, ts) <- map fold $ subsets tactics]
        matchups = [(p1n, p2n, domsMatch p1 p2 games seed) | (p1n, p1) <- players, (p2n, p2) <- players] `using` parList rdeepseq
    dumpTXT matchups
    dumpCSV matchups

dumpTXT :: [(String, String, Scores)] -> IO ()
dumpTXT matches = do
    h <- openFile "results.txt" WriteMode
    mapM_ (\(p1, p2, r) -> hPutStrLn h $ p1 ++ " vs " ++ p2 ++ ": " ++ show r) matches
    hClose h

dumpCSV :: [(String, String, Scores)] -> IO ()
dumpCSV matches = do
    h <- openFile "results.csv" WriteMode
    let rows = zipWith (:) players $ chunks (length players) [show $ winPercent r | (_, _, r) <- matches]
        players = take (round (sqrt . fromIntegral $ length matches :: Double)) [p | (_, p, _) <- matches]
    hPutStrLn h $ "," ++ intercalate "," players
    mapM_ (hPutStrLn h . intercalate ",") rows
    hClose h

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

winPercent :: (Int, Int) -> Double
winPercent (x, y) = fromIntegral x / fromIntegral (x + y) * 100.0