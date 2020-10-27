import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "dominos" $ do
    it "should be 28 dominos in total" $ do
      length dominos `shouldBe` 28

  describe "possPlays" $ do
    let board = [(5,2),(2,4),(4,4),(4,0)]
    it "returns possibilities on both ends of the board" $ do
      let hand = [(6,1),(6,5),(3,3),(0,0)]
      (possPlays board hand) `shouldBe` ([(6,5)],[(0,0)])