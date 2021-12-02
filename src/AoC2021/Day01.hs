module AoC2021.Day01 where

import           Solver (Solver)


indicator :: Ord n => n -> n -> Int
indicator a b
  | a < b = 1
  | otherwise = 0

inputAsList :: String -> [Int]
inputAsList = map read . lines

solve01 :: Solver
solve01 input = show . sum $ zipWith indicator ns (tail ns)
  where ns = inputAsList input

solve02 :: Solver
solve02 = const "Not implemented."
