module AoC2021.Day05Spec where

import           Test.Hspec

import           AoC2021.Day05

input01 :: String
input01 = "0,9 -> 5,9\n\
          \8,0 -> 0,8\n\
          \9,4 -> 3,4\n\
          \2,2 -> 2,1\n\
          \7,0 -> 7,4\n\
          \6,4 -> 2,0\n\
          \0,9 -> 2,9\n\
          \3,4 -> 1,4\n\
          \0,0 -> 8,8\n\
          \5,5 -> 8,2"

spec :: Spec
spec = do
  describe "day05-01" $ do
    it "correctly calculates example" $ do
      solve01 input01 `shouldBe` "5"


