module AoC2021.Day03Spec (spec) where

import           Test.Hspec    (Spec, describe, it, shouldBe)

import           AoC2021.Day03
import qualified Text.Parsec   as P

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

input02 :: String
input02 = input01

result01 :: String
result01 = "198"

result02 :: String
result02 = "230"

bitStrings02 :: [[Char]]
bitStrings02 = case P.runParser parseAllBitStrings () "" input02 of
  Right bss -> bss


spec :: Spec
spec = do
  describe "Day03-01" $ do
    it "solves example input" $ do
      solve01 input01 `shouldBe` result01
  describe "Day03-02" $ do
    it "has correct life support bit strings" $ do
      let (oxy,co2) = calculateLifeSupportBitStrings bitStrings02
      oxy `shouldBe` "10111"
      co2 `shouldBe` "01010"
    it "solves example input" $ do
      solve02 input02 `shouldBe` result02
