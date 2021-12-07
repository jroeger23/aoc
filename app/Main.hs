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


implementedSolvers :: [(String, Solver)]
implementedSolvers = [ ("day01-1", D01.solve01)
                     , ("day01-2", D01.solve02)
                     , ("day02-1", D02.solve01)
                     , ("day02-2", D02.solve02)
                     , ("day03-1", D03.solve01)
                     , ("day03-2", D03.solve02)
                     , ("day04-1", D04.solve01)
                     , ("day05-1", D05.solve01)
                     ]

printUsage :: IO ()
printUsage = putStrLn "Usage: ./aoc2021 <solver> <inputfile>"

firstTwoArgs :: IO (String, String)
firstTwoArgs = getArgs >>= \case
  (a:b:_) -> return (a,b)
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

main :: IO ()
main = do
  (nSolver, file) <- firstTwoArgs
  case lookup nSolver implementedSolvers of
    Just solver -> do
      putStrLn $ "Running solver " ++ nSolver ++ " on " ++ file
      runSolver solver file
    Nothing     -> do
      putStrLn "Solver not found. Implemented solvers are:"
      printSolvers
      exitFailure
