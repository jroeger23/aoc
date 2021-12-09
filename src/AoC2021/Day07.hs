module AoC2021.Day07 where

import           Data.Bifunctor               (bimap)
import           Data.List                    (sort)
import qualified Numeric.LinearAlgebra.Static as N
import qualified Parsers                      as P
import           Solver                       (Solver)


mean :: (Fractional n, Foldable f) => f n -> n
mean ns
  | null ns = 0
  | otherwise = uncurry (/) . foldl f (0,0) $ ns
  where f (t,c) n = (t+n,c+1)

median :: Ord a => [a] -> a
median as = let m = length as `div` 2
            in  sort as!!m

minFuelCost :: [Int] -> Int
minFuelCost ns = sum . map (abs . (median ns-)) $ ns


day07 :: ([Int] -> Int) -> Solver
day07 algo input = case P.runParser P.parseIntegerList () "" input of
  Left e          -> show e
  Right positions -> show . algo $ positions

solve01 :: Solver
solve01 = day07 minFuelCost
