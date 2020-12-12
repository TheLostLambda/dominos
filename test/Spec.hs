import Test.Hspec
import Test.QuickCheck

import AI
import Control.Monad (when)
import DomsMatch (DominoBoard (..), End (..), Player (..))
import qualified DomsMatch as DM
import Game
import Lib hiding (End (..))
import qualified Ref

shouldTestPlayers = True

main :: IO ()
main = hspec $ do
    describe "dominos" $ do
        it "should be 28 dominos in total" $ do
            length dominos `shouldBe` 28

    describe "possPlays" $ do
        let board = [(5, 2), (2, 4), (4, 4), (4, 0)]
        it "returns possibilities on both ends of the board" $ do
            let hand = [(6, 1), (6, 5), (3, 3), (0, 0)]
            possPlays board hand `shouldBe` ([(6, 5)], [(0, 0)])

    describe "scoreN" $ do
        let board = [(5, 2), (2, 4), (4, 4), (4, 0)]
        it "returns all possible plays with a score of N" $ do
            scoreN board 2 `shouldBe` ([(5, 5), (5, 6)], [(0, 1), (0, 5)])

    describe "playDomsRound" $ do
        it "matches the reference implementation" $
            property $
                \rng -> playDomsRound simplePlayer simplePlayer rng == Ref.playDomsRound Ref.simplePlayer Ref.simplePlayer rng

    describe "allPlays" $ do
        let hand = [(4, 4), (3, 0), (3, 2), (5, 1), (1, 1), (6, 6), (2, 0), (4, 2), (4, 0)]
        let board = Board (4, 1) (4, 1) [] -- Only (4,1) is on the board with no history
        it "returns all possible plays (both the domino and the end)" $ do
            allPlays (GS hand board undefined undefined) `shouldBe` [((4, 4), L), ((4, 2), L), ((4, 0), L), ((5, 1), R), ((1, 1), R)]

    describe "scorePlay" $ do
        let board = Board (4, 1) (4, 1) [] -- Only (4,1) is on the board with no history
        let plays = [((4, 4), L), ((4, 2), L), ((4, 0), L), ((5, 1), R), ((1, 1), R)]
        it "scores all possible plays" $ do
            map (scorePlay $ GS undefined board undefined undefined) plays `shouldBe` [3, 1, 0, 3, 2]

    describe "highScoring" $ do
        let hand = [(4, 4), (3, 0), (3, 2), (5, 1), (1, 1), (6, 6), (2, 0), (4, 2), (4, 0)]
        let board = Board (4, 1) (4, 1) [] -- Only (4,1) is on the board with no history
        it "returns possible plays and their score" $
            do
                highScoring (GS hand board undefined undefined) [((4, 4), L), ((4, 2), L), ((4, 0), L), ((5, 1), R), ((1, 1), R)]
                `shouldBe` [(((4, 4), L), 3), (((4, 2), L), 1), (((4, 0), L), 0), (((5, 1), R), 3), (((1, 1), R), 2)]

    when shouldTestPlayers $ do
        describe "playerH" $ do
            it "should win against the random player when going first" $
                property $
                    \rng -> let (f, s) = DM.domsMatch playerH DM.randomPlayer 200 rng in f > s
            it "and when going second" $
                property $
                    \rng -> let (f, s) = DM.domsMatch DM.randomPlayer playerH 200 rng in f < s

        describe "playerHFE" $ do
            it "should win against playerH when going first" $
                property $
                    \rng -> let (f, s) = DM.domsMatch playerHFE playerH 300 rng in f > s
            it "and when going second" $
                property $
                    \rng -> let (f, s) = DM.domsMatch playerH playerHFE 300 rng in f < s