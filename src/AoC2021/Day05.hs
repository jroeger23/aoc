module AoC2021.Day05 where

import           Data.Foldable   (Foldable (fold))
import qualified Data.Map.Strict as M
import           Parsers
import           Solver          (Solver)
import qualified Text.Parsec     as P


data Point a = Point a a deriving (Eq, Show, Ord)

type Line = (Point Int, Point Int)

newtype AdditiveGrid = AdditiveGrid { getAdditiveGrid :: M.Map (Point Int) Int }

instance Semigroup AdditiveGrid where
  (AdditiveGrid m1) <> (AdditiveGrid m2) = AdditiveGrid (M.unionWith (+) m1 m2)

instance Monoid AdditiveGrid where
  mempty = AdditiveGrid M.empty

parsePoint :: (Read i, Integral i) => Parser (Point i)
parsePoint = do
  x <- parseIntegral
  surroundBy (P.char ',') parseSpace
  Point x <$> parseIntegral

parseLine :: Parser Line
parseLine = do
  p1 <- parsePoint
  surroundBy (P.string "->") parseSpace
  p2 <- parsePoint
  return (p1, p2)


lineToGrid :: Line -> AdditiveGrid
lineToGrid (Point x1 y1, Point x2 y2)
  | y1 == y2 = gridFromList $ map (\x -> (Point x y1, 1)) (interval x1 x2)
  | x1 == x2 = gridFromList $ map (\y -> (Point x1 y, 1)) (interval y1 y2)
  | otherwise = mempty
  where interval a b = [min a b .. max a b]
        gridFromList = AdditiveGrid . M.fromDistinctAscList

twoPointOverlapCount :: [Line] -> Int
twoPointOverlapCount = sum . indicate . foldMap lineToGrid
  where indicate (AdditiveGrid m) = map (\x -> if x >= 2 then 1 else 0) $ M.elems m

solve01 :: Solver
solve01 input = case P.runP (parseLinesWith parseLine) () "Day05 - input" input of
  Left e   -> show e
  Right ls -> show (twoPointOverlapCount ls)

solve02 :: Solver
solve02 = undefined
