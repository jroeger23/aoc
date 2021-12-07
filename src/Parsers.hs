module Parsers where

import qualified Text.Parsec as P

type Parser = P.Parsec String ()

parseIntegral :: (Read i, Integral i) => Parser i
parseIntegral = read <$> P.many1 P.digit


parseSpace :: Parser String
parseSpace = P.many (P.char ' ')

parseSpace1 :: Parser String
parseSpace1 = P.many1 (P.char ' ')

parseLinesWith :: Parser a -> Parser [a]
parseLinesWith p = P.sepEndBy p (P.many1 P.endOfLine)


surroundBy :: Parser b -> Parser a -> Parser b
surroundBy p sur = do
  sur
  r <- p
  sur
  return r
