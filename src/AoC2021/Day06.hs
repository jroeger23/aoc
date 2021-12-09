module AoC2021.Day06 where

import           Data.Semigroup        (stimes)
import           Numeric.LinearAlgebra ((#>), (><))
import qualified Numeric.LinearAlgebra as M
import           Parsers               (Parser, parseIntegral)
import qualified Text.Parsec           as P

-- Linear (in growth days) time algorithm ----------------------------------------------------------
-- See Leslie Matrices (https://en.wikipedia.org/wiki/Leslie_matrix)

leslieMatrix :: M.Matrix Double -- Use double, to use optimized matrix multiplication of hmatrix
leslieMatrix = (9><9) [ 0, 1, 0, 0, 0, 0, 0, 0, 0 -- Count 1 -> 0
                      , 0, 0, 1, 0, 0, 0, 0, 0, 0 -- Count 2 -> 1
                      , 0, 0, 0, 1, 0, 0, 0, 0, 0 -- Count 3 -> 2
                      , 0, 0, 0, 0, 1, 0, 0, 0, 0 -- Count 4 -> 3
                      , 0, 0, 0, 0, 0, 1, 0, 0, 0 -- Count 5 -> 4
                      , 0, 0, 0, 0, 0, 0, 1, 0, 0 -- Count 6 -> 5,
                      , 1, 0, 0, 0, 0, 0, 0, 1, 0 -- Count 7 -> 6, 0 -> 6 (re-spawn, instead of dying)
                      , 0, 0, 0, 0, 0, 0, 0, 0, 1 -- Count 8 -> 7
                      , 1, 0, 0, 0, 0, 0, 0, 0, 0 -- Count 9 -> 8, 0 -> 8 (spawn child with 8)
                      ]

projectNDays :: Int -> M.Matrix Double -> M.Matrix Double
projectNDays = stimes

day0 :: [Int] -> M.Vector Double
day0 = M.vector . zipWith count [0..8] . repeat
  where count x = fromIntegral . length . filter (==x)

solve' :: Int -> [Int] -> Int
solve' days fishes = round $ M.sumElements dayN
  where dayN = projectNDays days leslieMatrix #> day0 fishes
----------------------------------------------------------------------------------------------------


-- Exponential (in growth days) time algorithm -----------------------------------------------------
-- | Calculate the number of fishes spawned by one fish within d days
f :: Int -> Int
f d = (1+) . sum . map (f . childOffset) $ [0 .. nSpawn - 1]
  where
    childOffset i = d-9-(7*i)
    nSpawn = ceiling $ fromIntegral d / 7


-- | Total number of fishes within d days, given a list of fishes with their initial counter
solve :: Int -> [Int] -> Int
solve d = sum . map (f . (d-))
----------------------------------------------------------------------------------------------------

parseIntegerList :: Parser [Int]
parseIntegerList = P.sepBy parseIntegral (P.char ',')

solve01 :: String -> String
solve01 input = case P.runParser parseIntegerList () "" input of
  Left e   -> show e
  Right is -> show (solve' 80 is)


-- | Way to inefficient, maybe solve the sum of f?
solve02 :: String -> String
solve02 input = case P.runParser parseIntegerList () "" input of
  Left e   -> show e
  Right is -> show (solve' 256 is)
