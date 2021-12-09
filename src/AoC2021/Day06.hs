module AoC2021.Day06 where

import           Parsers
import qualified Text.Parsec as P

-- | Calculate the number of fishes spawned by one fish within d days
f :: Int -> Int
f d = (1+) . sum . map (f . childOffset) $ [0 .. nSpawn - 1]
  where
    childOffset i = d-9-(7*i)
    nSpawn = ceiling $ fromIntegral d / 7


-- | Total number of fishes within d days, given a list of fishes with their initial counter
solve :: Int -> [Int] -> Int
solve d = sum . map (f . (d-))

parseIntegerList :: Parser [Int]
parseIntegerList = P.sepBy parseIntegral (P.char ',')

solve01 :: String -> String
solve01 input = case P.runParser parseIntegerList () "" input of
  Left e   -> show e
  Right is -> show (solve 80 is)


-- | Way to inefficient, maybe solve the sum of f?
solve02 :: String -> String
solve02 input = case P.runParser parseIntegerList () "" input of
  Left e   -> show e
  Right is -> show (solve 256 is)
