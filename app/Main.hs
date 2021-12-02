{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad      (forM_)
import           Solver             (Solver)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (IOMode (ReadMode), hClose, hGetContents,
                                     openFile)

import qualified AoC2021.Day01      as D01


implementedSolvers :: [(String, Solver)]
implementedSolvers = [ ("day01-1", D01.solve01)
                     , ("day01-2", D01.solve02)
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
  putStr $ solver contents
  hClose handle

printSolvers :: IO ()
printSolvers = forM_ (map fst implementedSolvers) (putStrLn.(" - "<>))

main :: IO ()
main = do
  (solver, file) <- firstTwoArgs
  case lookup solver implementedSolvers of
    Just solver -> runSolver solver file
    Nothing     -> do
      putStrLn "Solver not found. Implemented solvers are:"
      printSolvers
      exitFailure
