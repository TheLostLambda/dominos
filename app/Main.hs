module Main where

import DomsMatch
import Interactive

main :: IO ()
main = do
    (man, ai) <- domsMatch human (documentPlayer randomPlayer) 1 42
    if man > ai
        then putStrLn "You vanquished my dominos AI!"
        else putStrLn "Better luck next time..."