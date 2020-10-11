module Unit.One where

import Test.Hspec

test :: IO ()
test = hspec $ do
  describe "one plus one" $ do
    it "should be 2" $ do
      (1 + 1) `shouldBe` (2 :: Int)
    