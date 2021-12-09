module Parsers (
  module Parsers,
  module Text.Parsec
) where

import           Text.Parsec

type Parser = Parsec String ()

parseIntegral :: (Read i, Integral i) => Parser i
parseIntegral = read <$> many1 digit

parseSpace :: Parser String
parseSpace = many (char ' ')

parseSpace1 :: Parser String
parseSpace1 = many1 (char ' ')

parseLinesWith :: Parser a -> Parser [a]
parseLinesWith p = sepEndBy p (many1 endOfLine)

parseIntegerList :: Parser [Int]
parseIntegerList = sepBy parseIntegral (char ',')


surroundBy :: Parser b -> Parser a -> Parser b
surroundBy p sur = do
  sur
  r <- p
  sur
  return r
