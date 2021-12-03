module AoC2021.Day02 where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Function       ((&))
import           Data.Functor        ((<&>))
import           Solver              (Solver)
import qualified Text.Parsec         as P

data Position a = Position { positionX :: a
                           , positionY :: a }

data Pose a = Pose { posePosition :: Position a
                   , poseAim      :: a }

type Move a = Position a -> Position a

type Move' a = Pose a -> Pose a

type Parser = P.Parsec String ()

parseMove :: (Read a, Num a) => Parser (Move a)
parseMove = parseUp <|> parseDown <|> parseForward <|> parseBackward
  where
    parseUp = P.string "up" >> P.spaces >> num <&>             \n (Position x y) -> Position x (y-n)
    parseDown = P.string "down" >> P.spaces >> num <&>         \n (Position x y) -> Position x (y+n)
    parseForward = P.string "forward" >> P.spaces >> num <&>   \n (Position x y) -> Position (x+n) y
    parseBackward = P.string "backward" >> P.spaces >> num <&> \n (Position x y) -> Position (x-n) y
    num = P.many1 P.digit <&> read

parseMove' :: (Read a, Num a) => Parser (Move' a)
parseMove' = P.choice [ f "up" $                                   \n (Pose pos a) -> Pose pos (a-n)
                      , f "down" $                                 \n (Pose pos a) -> Pose pos (a+n)
                      , f "forward" $  \n (Pose (Position x y) a) -> Pose (Position (x+n) (y+a*n)) a
                      , f "backward" $ \n (Pose (Position x y) a) -> Pose (Position (x+n) (y-a*n)) a
                      ]
  where
    f name mkMove = P.string name >> P.spaces >> num <&> mkMove
    num = P.many1 P.digit <&> read

parseFn1EachLine :: Parser (a -> a) -> Parser [a -> a]
parseFn1EachLine p = P.many $ do
  m <- p <|> (P.space >> return id) -- empty move for empty lines
  P.spaces
  return m


evaluateMoves :: Num a => [Move a] -> Position a
evaluateMoves = foldl (&) (Position 0 0)

evaluateMoves' :: Num a => [Move' a] -> Pose a
evaluateMoves' = foldl (&) (Pose (Position 0 0) 0)

solve01 :: Solver
solve01 input = case P.runParser (parseFn1EachLine parseMove) () "Day02-01" input of
  Left e   -> show e
  Right ms ->  let (Position x y) = evaluateMoves ms
               in  show $ x*y

solve02 :: Solver
solve02 input = case P.runParser (parseFn1EachLine parseMove') () "Day02-01" input of
  Left e   -> show e
  Right ms ->  let (Pose (Position x y) _) = evaluateMoves' ms
               in  show $ x*y
