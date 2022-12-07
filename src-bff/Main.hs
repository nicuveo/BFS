{- Small formatting program for Brainfuck.

Reads brainfuck code from the standard input, runs the optimizer, and
formats the output.
-}

module Main where

import Data.Foldable
import Data.List.Split
import Safe
import System.Environment
import System.Exit
import System.IO
import System.IO.Strict   as IO

import Misc
import Optimizer

help :: IO a
help = hPutStrLn stderr "usage: bff [-n columns=100]" >> exitFailure

options :: IO Int
options = do
  args <- getArgs
  case args of
    ["-n", x] -> readMay x `onNothing` help
    []        -> pure 100
    _         -> help

main :: IO ()
main = do
  columns <- options
  inputBF <- IO.getContents
  traverse_ putStrLn $ chunksOf columns $ safeOptimize inputBF
