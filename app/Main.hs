{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad      (forM_)
import           Solver             (Solver)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (IOMode (ReadMode), hClose, hGetContents,
                                     openFile)

import qualified AoC2021.Day01      as D01
import qualified AoC2021.Day02      as D02
import qualified AoC2021.Day03      as D03
import qualified AoC2021.Day04      as D04
import qualified AoC2021.Day05      as D05
import qualified AoC2021.Day06      as D06
import qualified AoC2021.Day07      as D07
import qualified AoC2021.Day08      as D08
import           Data.Maybe         (fromMaybe)


implementedSolvers :: [(String, Solver)]
implementedSolvers = [ ("day01-1", D01.solve01)
                     , ("day01-2", D01.solve02)
                     , ("day02-1", D02.solve01)
                     , ("day02-2", D02.solve02)
                     , ("day03-1", D03.solve01)
                     , ("day03-2", D03.solve02)
                     , ("day04-1", D04.solve01)
                     , ("day05-1", D05.solve01)
                     , ("day06-1", D06.solve01)
                     , ("day06-2", D06.solve02)
                     , ("day07-1", D07.solve01)
                     , ("day08-1", D08.solve01)
                     , ("day08-2", D08.solve02)
                     ]

printUsage :: IO ()
printUsage = putStrLn "Usage: ./aoc2021 <solver> [inputfile]"

aocArgs :: IO (String, Maybe String)
aocArgs = getArgs >>= \case
  (a:b:_) -> return (a,Just b)
  (a:_)   -> putStrLn "No input given. Assuming location..." >> return (a, Nothing )
  _       -> printUsage >> exitFailure

runSolver :: Solver -> String -> IO ()
runSolver solver file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  putStrLn "---- Output ----"
  putStrLn $ solver contents
  putStrLn "----------------"
  hClose handle

printSolvers :: IO ()
printSolvers = forM_ (map fst implementedSolvers) (putStrLn.(" - "<>))

inferFileFromSolver :: String -> String
inferFileFromSolver s = let d = take 5 s
                        in if length s >= 5
                            then "inputs/" ++ d
                            else ""

main :: IO ()
main = do
  (nSolver, mFile) <- aocArgs
  let file = fromMaybe (inferFileFromSolver nSolver) mFile
  case lookup nSolver implementedSolvers of
    Just solver -> do
      putStrLn $ "Running solver " ++ nSolver ++ " on " ++ file
      runSolver solver file
    Nothing     -> do
      putStrLn "Solver not found. Implemented solvers are:"
      printSolvers
      exitFailure
