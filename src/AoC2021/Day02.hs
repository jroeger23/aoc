module AoC2021.Day02 where

import           Control.Applicative ((<|>))
import           Data.Functor        ((<&>))
import           Solver              (Solver)
import qualified Text.Parsec         as P

data Position a = Position { positionX :: a
                           , positionY :: a }

type Move a = Position a -> Position a

type Parser = P.Parsec String ()

parseMove :: (Read a, Num a) => Parser (Move a)
parseMove = parseUp <|> parseDown <|> parseForward <|> parseBackward
  where
    parseUp = P.string "up" >> P.spaces >> num <&>             \n (Position x y) -> Position x (y-n)
    parseDown = P.string "down" >> P.spaces >> num <&>         \n (Position x y) -> Position x (y+n)
    parseForward = P.string "forward" >> P.spaces >> num <&>   \n (Position x y) -> Position (x+n) y
    parseBackward = P.string "backward" >> P.spaces >> num <&> \n (Position x y) -> Position (x-n) y
    num = P.many1 P.digit <&> read

parseAllMoves :: (Read a, Num a) => Parser [Move a]
parseAllMoves = P.many $ do
  m <- parseMove <|> (P.space >> return id) -- empty move for empty lines
  P.spaces
  return m


evaluateMoves :: Num a => [Move a] -> Position a
evaluateMoves = foldl (flip ($)) (Position 0 0)

solve01 :: Solver
solve01 input = case P.runParser parseAllMoves () "Day02-01" input of
  Left e   -> show e
  Right ms ->  let (Position x y) = evaluateMoves ms
               in  show $ x*y
