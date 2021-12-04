module AoC2021.Day03 where

import           Data.Char
import           Data.Functor ((<&>))
import           Solver       (Solver)
import qualified Text.Parsec  as P


-- Running count of densities for each column of bits
type Densities = [Int]

type Parser = P.Parsec String ()

weight :: Char -> Int
weight '0' = -1
weight '1' = 1
weight _   = 0

dominantBitFromDensity :: Int -> Char
dominantBitFromDensity d
  | d >= 0    = '1'
  | otherwise = '0'

flipBit :: Char -> Char
flipBit '0' = '1'
flipBit '1' = '0'
flipBit c   = c

bin2Int :: String -> Int
bin2Int = foldl (\acc x -> acc*2 + digitToInt x) 0

-- Apply bit wise weight of a new bitString to accumulated densities
applyDensities :: Densities -> [Char] -> Densities
applyDensities ds = zipWith (+) (ds ++ repeat 0) . map weight

-- Accumulate column-wise densities
calculateColumnWiseDensities :: [[Char]] -> Densities
calculateColumnWiseDensities = foldl applyDensities []

parseBitString :: Parser [Char]
parseBitString = P.many $ P.oneOf "01"

parseAllBitStrings :: Parser [[Char]]
parseAllBitStrings = P.many $ do
  bs <- parseBitString
  P.endOfLine
  return bs


solve01 :: Solver
solve01 input = case P.runParser parseAllBitStrings () "" input <&> calculateColumnWiseDensities of
  Left e -> show e
  Right densities -> let gammaStr   = map dominantBitFromDensity densities
                         epsilonStr = map flipBit gammaStr
                     in show $ bin2Int gammaStr * bin2Int epsilonStr

solve02 :: Solver
solve02 = undefined
