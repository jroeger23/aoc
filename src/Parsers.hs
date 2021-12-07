module Parsers where

import qualified Text.Parsec as P

type Parser = P.Parsec String ()

parseIntegral :: (Read i, Integral i) => Parser i
parseIntegral = read <$> P.many1 P.digit
