module AoC2021.Day04Spec where

import           AoC2021.Day04
import qualified Data.IntMap   as M
import qualified Data.IntSet   as S
import           Test.Hspec
import qualified Text.Parsec   as P

input01 :: String
input01 = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
          \22 13 17 11  0\n\
          \ 8  2 23  4 24\n\
          \21  9 14 16  7\n\
          \ 6 10  3 18  5\n\
          \ 1 12 20 15 19\n\
          \              \n\
          \ 3 15  0  2 22\n\
          \ 9 18 13 17  5\n\
          \19  8  7 25 23\n\
          \20 11 10 24  4\n\
          \14 21 16 12  6\n\
          \              \n\
          \14 21 17 24  4\n\
          \10 16 15  9 19\n\
          \18  8 23 26 20\n\
          \22 11 13  6  5\n\
          \ 2  0 12  3  7\n"

parsedInput01 :: ([Int], [Board])
parsedInput01 = parseNoError parseAll input01

sampleBoard :: Board
sampleBoard = M.fromList . flip zip [1..] . map (*2) $ [1..25]

parseNoError :: Parser a -> String -> a
parseNoError p str = case P.runParser p  () "TestSrc" str of
  Right b -> b

sampleBoardParsed :: Board
sampleBoardParsed = parseNoError parseBoard "2 4 6 8 10\n\
                                            \12 14 16 18 20\n\
                                            \22 24 26 28 30\n\
                                            \32 34 36 38 40\n\
                                            \42 44 46 48 50\n"

stepAllSpec :: Spec
stepAllSpec = do
  describe "step all works as expected" $ do
    it "returns a winner if there is one" $ do
      let winner = stepAll 50 [ (sampleBoard, S.fromList [1,2,3,4])
                              , (sampleBoard, S.fromList [1,7,13,19])
                              , (sampleBoard, S.fromList [21..24])
                              ]
      winner `shouldBe` Left (sum $ map (*2) [1..20])
    it "returns new marks if there is no winner" $ do
      let noWinner = stepAll 50 [ (sampleBoard, S.fromList [1,2,3,4])
                                , (sampleBoard, S.fromList [1,7,13,19])
                                , (sampleBoard, S.fromList [21..23])
                                ]
      noWinner `shouldBe` Right [ S.fromList [1,2,3,4,25]
                                , S.fromList [1,7,13,19,25]
                                , S.fromList [21,22,23,25]
                                ]

boardParseSpec :: Spec
boardParseSpec = do
  describe "board parses successfully" $ do
    it "parses sample board" $ do
      sampleBoardParsed `shouldBe` sampleBoard

stepBoardSpec :: Spec
stepBoardSpec = do
  describe "step board succeeds" $ do
    it "detects wins" $ do
      stepBoard (2*5) sampleBoard (S.fromList [1..4]) `shouldBe` Left (sum $ map (*2) [6..25])
    it "steps" $ do
      stepBoard (2*21) sampleBoard (S.fromList [1..4]) `shouldBe` Right (S.fromList $ 21:[1..4])

winningMarksSpec :: Spec
winningMarksSpec = do
  describe "winningMarks are consistent"  $ do
    it "misses no configuration" $ do
      winningMarks `shouldContain` [ S.fromList [1,2,3,4,5]
                                   , S.fromList [6,7,8,9,10]
                                   , S.fromList [11,12,13,14,15]
                                   , S.fromList [16,17,18,19,20]
                                   , S.fromList [21,22,23,24,25]
                                   , S.fromList [1,6,11,16,21]
                                   , S.fromList [2,7,12,17,22]
                                   , S.fromList [3,8,13,18,23]
                                   , S.fromList [4,9,14,19,24]
                                   , S.fromList [5,10,15,20,25]
                                   ]
    it "has no extra configuration" $ length winningMarks `shouldBe` 10

spec :: Spec
spec = do
  describe "Day04-1 correctly calculates example" $ do
    stepBoardSpec
    winningMarksSpec
    boardParseSpec
    stepAllSpec
    it "parses example input as expected" $ do
      let (ns, bs) = parsedInput01
      length bs `shouldBe` 3
      ns `shouldBe` [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
      M.lookup 22 (head bs)  `shouldBe` Just 1
      M.lookup 3 (bs!!1)  `shouldBe` Just 1
      M.lookup 14 (bs!!2)  `shouldBe` Just 1
      M.lookup 14 (head bs)  `shouldBe` Just 13
      M.lookup 7 (bs!!1)  `shouldBe` Just 13
      M.lookup 23 (bs!!2)  `shouldBe` Just 13

    it "returns 4512" $ do
      solve01 input01 `shouldBe` "4512"


