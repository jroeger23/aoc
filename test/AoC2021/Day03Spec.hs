module AoC2021.Day03Spec (spec) where

import           Test.Hspec    (Spec, describe, it, shouldBe)

import           AoC2021.Day03 (solve01)

input01 :: String
input01 = "00100\n\
          \11110\n\
          \10110\n\
          \10111\n\
          \10101\n\
          \01111\n\
          \00111\n\
          \11100\n\
          \10000\n\
          \11001\n\
          \00010\n\
          \01010\n"

result01 :: String
result01 = "198"

spec :: Spec
spec = do
  describe "Day03-01" $ do
    it "solves example input" $ do
      solve01 input01 `shouldBe` result01
