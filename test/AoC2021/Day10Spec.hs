module AoC2021.Day10Spec where

import           Test.Hspec

import           AoC2021.Day10

input01 :: String
input01 = "[({(<(())[]>[[{[]{<()<>>\n\
          \[(()[<>])]({[<{<<[]>>(\n\
          \{([(<{}[<>[]}>{[]{[(<()>\n\
          \(((({<>}<{<{<>}{[]{[]{}\n\
          \[[<[([]))<([[{}[[()]]]\n\
          \[{[{({}]{}}([{[{{{}}([]\n\
          \{<[[]]>}<{[{[{[]{()[[[]\n\
          \[<(<(<(<{}))><([]([]()\n\
          \<{([([[(<>()){}]>(<<{{\n\
          \<{([{{}}[<[[[<>{}]]]>[]]\n"

line1 :: (String, Char)
line1 = ("{([(<{}[<>[]}>{[]{[(<()>", '}')

line2 :: (String, Char)
line2 = ("[[<[([]))<([[{}[[()]]]", ')')

line3 :: (String, Char)
line3 = ("[{[{({}]{}}([{[{{{}}([]", ']')

line4 :: (String, Char)
line4 = ("[<(<(<(<{}))><([]([]()", ')')

line5 :: (String, Char)
line5 = ("<{([([[(<>()){}]>(<<{{", '>')

lineWiseCheck :: (String, Char) -> Spec
lineWiseCheck (l, r) = do
  it ("firstInvalid returns " ++ [r] ++ " on " ++ l) $ do
   firstInvalid l `shouldBe` Just r


spec :: Spec
spec = do
  describe "Day10-1 example" $ do
    it "returns 26397 on example" $ do
      solve01 input01 `shouldBe` "26397"
  describe "firstInvalid check" $ do
    lineWiseCheck line1
    lineWiseCheck line2
    lineWiseCheck line3
    lineWiseCheck line4
    lineWiseCheck line5
