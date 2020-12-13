module Main where

import DomsMatch
import AI

main :: IO ()
main = undefined

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)