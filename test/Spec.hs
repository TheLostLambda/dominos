import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "dominos" $ do
    it "should be 28 dominos in total" $ do
      length dominos `shouldBe` 28
