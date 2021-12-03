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


implementedSolvers :: [(String, Solver)]
implementedSolvers = [ ("day01-1", D01.solve01)
                     , ("day01-2", D01.solve02)
                     , ("day02-1", D02.solve01)
                     , ("day02-2", D02.solve02)
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
