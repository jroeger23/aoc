module AoC2021.Day03 where

import           Control.Monad             (forM_, unless, when)
import qualified Control.Monad.Trans.State as S
import           Data.Array                ((!))
import qualified Data.Array                as A
import           Data.Char                 (digitToInt)
import           Data.Functor              ((<&>))
import           Data.List                 (partition)
import           Solver                    (Solver)
import qualified Text.Parsec               as P

-- Running count of densities for each column of bits
type Densities = [Int]
type Parser = P.Parsec String ()

-- State to keep during LifeSupport calculation
data LSCalcState = LSCalcState
  { oxygenDensities   :: Densities
  , co2Densities      :: Densities
  , oxygenBitStrings  :: [A.Array Int Char]
  , co2BitStrings     :: [A.Array Int Char]
  , lsCalcBitIndex    :: Int
  , lsCalcBitIndexMax :: Int }

-- LifeSupport calculation action
type LSCalcAction = S.State LSCalcState

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

indexBitString :: [Char] -> A.Array Int Char
indexBitString bs = A.listArray (1,n) bs
  where n = length bs

-- Modify a density count, as if the given bit string were never part of it
removeFromDensities :: Densities -> String -> Densities
removeFromDensities ds = applyDensities ds . map flipBit

removeAllFromDensities :: Densities -> [String] -> Densities
removeAllFromDensities = foldl removeFromDensities

-- Remove BitStrings from oxygenBitStrings according to the dominant bit rule
-- and adjust densities of the list accordingly
filterOxygenListAction :: LSCalcAction ()
filterOxygenListAction = do
  i <- S.gets lsCalcBitIndex
  d <- S.gets oxygenDensities
  bss <- S.gets oxygenBitStrings
  let requiredBit = if 0 <= (d!!i-1)
                      then '1' 
                      else '0'
  let (bss', rm) = partition (\a -> (a!i) == requiredBit) bss
  S.modify $ \s -> s { oxygenDensities = removeAllFromDensities d (map A.elems rm)
                     , oxygenBitStrings = bss' }

-- Remove BitStrings from co2BitStrings according to the dominant bit rule
-- and adjust densities of the list accordingly
filterCo2ListAction :: LSCalcAction ()
filterCo2ListAction = do
  i <- S.gets lsCalcBitIndex
  d <- S.gets co2Densities
  bss <- S.gets co2BitStrings
  let requiredBit = if 0 >= (d!!i-1)
                      then '0' 
                      else '1'
  let (bss', rm) = partition (\a -> (a!i) == requiredBit) bss
  S.modify $ \s -> s { co2Densities = removeAllFromDensities d (map A.elems rm)
                     , co2BitStrings = bss' }

calculateLifeSupportAction :: LSCalcAction ()
calculateLifeSupportAction = do
  oxygenDone <- S.gets $ (<=1) . length . oxygenBitStrings
  co2Done <- S.gets $ (<=1) . length . co2BitStrings
  unless oxygenDone filterOxygenListAction
  unless co2Done filterCo2ListAction
  unless (co2Done && oxygenDone) $ do
    i <- S.gets lsCalcBitIndex
    n <- S.gets lsCalcBitIndexMax
    S.modify $ \s -> s { lsCalcBitIndex = i+1 }
    unless (i==n) calculateLifeSupportAction


calculateLifeSupportBitStrings :: [[Char]] -> ([Char], [Char])
calculateLifeSupportBitStrings bss = (oxygenBs, co2Bs)
  where
    d = calculateColumnWiseDensities bss
    n = length d
    bss' = map indexBitString bss
    initialState = LSCalcState d d bss' bss' 1 n
    endState = S.execState calculateLifeSupportAction initialState
    oxygenBs = A.elems . head . oxygenBitStrings $ endState
    co2Bs = A.elems . head . co2BitStrings $ endState



solve02 :: Solver
solve02 input = case P.runParser parseAllBitStrings () "" input of
  Left e -> show e
  Right bss -> let (oxyStr, co2Str) = calculateLifeSupportBitStrings bss
               in show $ bin2Int oxyStr * bin2Int co2Str
