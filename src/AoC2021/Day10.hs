{-# LANGUAGE LambdaCase #-}
module AoC2021.Day10 where

import           Control.Applicative ((<|>))
import           Control.Monad       (foldM, void)
import           Data.Functor        ((<&>))
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)
import qualified Parsers             as P
import           Solver              (Solver)


rateBracket :: Char -> Int
rateBracket = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

parseAnyChunk :: P.Parser (Maybe Char)
parseAnyChunk = P.choice [ P.try $ parseChunk '(' ')'
                         , P.try $ parseChunk '[' ']'
                         , P.try $ parseChunk '{' '}'
                         , P.try $ parseChunk '<' '>'
                         ]

parseChunk :: Char -> Char -> P.Parser (Maybe Char)
parseChunk o c = P.parserTraced ("parseChunk " ++ [o,c]) $ do
  P.char o
  P.optionMaybe (P.try parseChunks) >>= \case
    Just (Just a) -> return (Just a)
    _             -> P.choice [ P.try $ P.char c >> return Nothing
                              , P.try $ Just <$> P.anyChar ]

parseChunks :: P.Parser (Maybe Char)
parseChunks = P.optionMaybe (P.try parseAnyChunk) >>= \case
  Just (Just a) -> return (Just a)
  Just Nothing  -> parseChunks
  Nothing       -> P.eof >> return Nothing

firstInvalid :: String -> Maybe Char
firstInvalid s = case P.runParser parseChunks () "" s of
  Left e  -> trace (show e) Nothing
  Right m -> m

solve01 :: Solver
solve01 = show . sum . map (maybe 0 rateBracket . firstInvalid) . lines

solve02 :: Solver
solve02 = undefined
