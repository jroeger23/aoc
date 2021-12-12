module AoC2021.Day08 where

import           Data.Char  (intToDigit, toUpper)
import           Data.List  (nub, permutations, sort)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set   as S
import qualified Parsers    as P
import           Solver     (Solver)



data Segment = A | B | C | D | E | F | G deriving (Read, Eq, Show, Bounded, Enum, Ord)

newtype Permutation = Permutation { getPermutation :: M.Map Segment Segment } deriving (Show, Eq)

instance Semigroup Permutation where
  p1 <> p2 = fromMaybe (Permutation M.empty) $ unifyPermutations p1 p2

instance Monoid Permutation where
  mempty = Permutation M.empty

newtype PossiblePermutations =
  PossiblePermutations { getPossiblePermutations :: [Permutation] } deriving(Show, Eq)

instance Semigroup PossiblePermutations where
  (<>) = productPermutations

instance Monoid PossiblePermutations where
  mempty = PossiblePermutations []

requireInjective :: Permutation -> Maybe Permutation
requireInjective (Permutation p) = if (S.size . S.fromList $ M.elems p) == M.size p
                                    then Just (Permutation p)
                                    else Nothing

unifyPermutations :: Permutation -> Permutation -> Maybe Permutation
unifyPermutations (Permutation p1) (Permutation p2)
  = sequenceA (M.unionWith uniqueKey (Just <$> p1) (Just <$> p2))
  >>= requireInjective . Permutation
  where uniqueKey s1 s2 = if s1 == s2 then s1 else Nothing

productPermutations :: PossiblePermutations -> PossiblePermutations -> PossiblePermutations
productPermutations (PossiblePermutations []) other = other
productPermutations other (PossiblePermutations []) = other
productPermutations (PossiblePermutations ps1) (PossiblePermutations ps2)
  = PossiblePermutations . filter (/= mempty) $ [p1 <> p2 | p1 <- ps1, p2 <- ps2]



segmentPermutations :: [Segment] -> [Segment] -> PossiblePermutations
segmentPermutations from = PossiblePermutations
                         . map (Permutation . M.fromList . zip from)
                         . permutations

deducePossiblePermutations :: [[Segment]] -> PossiblePermutations
deducePossiblePermutations = foldMap opts
  where opts s = case length s of
                  2 -> segmentPermutations s [C, F] -- Options for 1
                  3 -> segmentPermutations s [A, C, F] -- Options for 7
                  4 -> segmentPermutations s [B, C, D, F] -- Options for 4
                  5 -> segmentPermutations s [A, C, D, E, G] -- Options for 2
                   !+! segmentPermutations s [A, C ,D, F, G] -- Options for 3
                   !+! segmentPermutations s [A, B ,D, F, G] -- Options for 5
                  6 -> segmentPermutations s [A, B, C, E, F, G] -- Options for 0
                   !+! segmentPermutations s [A, B ,D, E, F, G] -- Options for 6
                   !+! segmentPermutations s [A, B ,C, D, F, G] -- Options for 9
                  _ -> PossiblePermutations [] -- Options for 8 retain 0 information
        (PossiblePermutations a) !+! (PossiblePermutations b) = PossiblePermutations $ a ++ b

decodeWithPermutation :: Permutation -> [Segment] -> Maybe Int
decodeWithPermutation (Permutation p) s = traverse (`M.lookup` p) s >>= f.sort
  where f [A,B,C,E,F,G]   = Just 0
        f [C,F]           = Just 1
        f [A,C,D,E,G]     = Just 2
        f [A,C,D,F,G]     = Just 3
        f [B,C,D,F]       = Just 4
        f [A,B,D,F,G]     = Just 5
        f [A,B,D,E,F,G]   = Just 6
        f [A,C,F]         = Just 7
        f [A,B,C,D,E,F,G] = Just 8
        f [A,B,C,D,F,G]   = Just 9
        f _               = Nothing

solveLine :: [[Segment]] -> [[Segment]] -> Either String [Int]
solveLine observed output = perms >>= \p -> traverse (decode p) output
  where
    decode p s = case decodeWithPermutation p s of
                  Just v  -> Right v
                  Nothing -> Left "Decoding error"
    perms = case deducePossiblePermutations observed of
              PossiblePermutations [p] -> Right p
              PossiblePermutations []  -> Left "No possible permutations"
              PossiblePermutations _   -> Left "Ambiguous observations"

solve01 :: Solver
solve01 input = case P.runParser parseDay08 () "" input of
  Left e -> show e
  Right lines -> case traverse (uncurry solveLine) lines of
    Left e        -> e
    Right results -> show . length . filter (`elem` [1,4,7,8]) $ concat results

solve02 :: Solver
solve02 input = case P.runParser parseDay08 () "" input of
  Left e -> show e
  Right lines -> case traverse (uncurry solveLine) lines of
    Left e        -> e
    Right results -> show . sum . map unDigits4 $ results
  where
    unDigits4 [a,b,c,d] = a*1000 + b*100 + c * 10 + d

-- Parsing stuff -----------------------------------------------------------------------------------
parseSegments :: P.Parser [Segment]
parseSegments = P.many1 . P.choice $ [ P.char 'a' >> return A
                                     , P.char 'b' >> return B
                                     , P.char 'c' >> return C
                                     , P.char 'd' >> return D
                                     , P.char 'e' >> return E
                                     , P.char 'f' >> return F
                                     , P.char 'g' >> return G
                                     ]

parseDay08 :: P.Parser [([[Segment]], [[Segment]])]
parseDay08 = P.sepEndBy line P.endOfLine
  where line = do
          observed <- P.sepEndBy1 parseSegments P.parseSpace1
          P.char '|'
          P.parseSpace
          output <- P.sepBy1 parseSegments P.parseSpace1
          return (observed, output)
