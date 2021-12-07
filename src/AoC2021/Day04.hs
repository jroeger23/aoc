{-# LANGUAGE LambdaCase #-}
module AoC2021.Day04 where

import           Solver         (Solver)

import           Control.Monad
import           Data.Bifunctor (Bifunctor (first), bimap)
import           Data.Functor   ((<&>))
import qualified Data.IntMap    as M
import qualified Data.IntSet    as S
import           Data.Maybe     (fromMaybe)
import           Debug.Trace
import           Parsers
import qualified Text.Parsec    as P


-- map each bingo number to a cell 1..25 from left to right, top to bottom
type Board = M.IntMap Int
-- marked cell indices
type Marks = S.IntSet


endByOnce :: Parser a -> Parser b -> Parser a
endByOnce p end = do
  a <- p
  end
  return a

parseBingoNumbers :: Parser [Int]
parseBingoNumbers = P.sepBy1 parseIntegral (P.char ',')

parseBoard :: Parser Board
parseBoard = M.fromList . flip zip [1..] . concat <$> parseGrid
  where
    parseLine = parseSpace >> replicateM 5 (endByOnce parseIntegral parseSpace)
    parseGrid = replicateM 5 (endByOnce parseLine P.endOfLine)
    parseSpace = P.many (P.char ' ')

parseAll :: Parser ([Int], [Board])
parseAll = do
  ns <- parseBingoNumbers
  P.many1 P.endOfLine
  bs <- P.sepEndBy1 parseBoard P.spaces
  return (ns, bs)

winningMarks :: [Marks]
winningMarks = S.fromDistinctAscList <$> (rows ++ cols)
  where
    rows = [ (+(i*5))  <$> [1..5] | i <- [0..4]]
    cols = [ (+i).(*5) <$> [0..4] | i <- [1..5]]


-- Left (Win with sum of unmarked numbers), Right (new marks)
stepBoard :: Int -> Board -> Marks -> Either Int Marks
stepBoard n b m =
  if any (`S.isSubsetOf` m') winningMarks
    then Left val
    else Right m'
  where
    m' = maybe m (`S.insert` m) (M.lookup n b)
    val = sum . map fst . filter (flip S.notMember m' . snd) $ M.toList b

stepAll :: Int -> [(Board, Marks)] -> Either Int [Marks]
stepAll n = traverse $ uncurry (stepBoard n)

playBingo :: [Int] -> [Board] -> Maybe Int
playBingo numbers boards = case f numbers initialMarks of
  Left result -> Just result
  Right _     -> Nothing
  where
    f :: [Int] -> [Marks] -> Either Int [Marks]
    f (n:ns) marks    = first (*n) (stepAll n (zip boards marks)) >>= f ns
    f []        marks = Right marks

    initialMarks = repeat S.empty

solve01 :: Solver
solve01 input = case P.runParser parseAll () "" input of
  Left e                  -> show e
  Right (numbers, boards) -> maybe "No Winner" show (playBingo numbers boards)
