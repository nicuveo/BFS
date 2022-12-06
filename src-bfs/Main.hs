module Main where

import Data.Foldable
import Data.List          qualified as L
import Data.These
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Assembler
import BuiltIn
import Compiler

fileResolver :: String -> IO (Maybe (String, String))
fileResolver "Prelude" = Just . ("Prelude.bs",) <$> preludeFile
fileResolver filename  = do
  let rfn = if ".bs" `L.isSuffixOf` filename
            then filename
            else filename ++ ".bs"
  dfe <- doesFileExist rfn
  if dfe
    then Just . (rfn,) <$> readFile rfn
    else return Nothing

help :: IO a
help = hPutStrLn stderr "usage: bfs [-O] file.bs" >> exitFailure

options :: IO (Bool, String)
options = do
  args <- getArgs
  case args of
    ["-O", f] -> return (True, f)
    [      f] -> return (False, f)
    _         -> help

main :: IO ()
main = do
  (dense, source) <- options
  result <- compile fileResolver source
  case result of
    This errors -> do
      report errors
      exitFailure
    That objects ->
      assemble dense objects
    These warnings objects -> do
      report warnings
      assemble dense objects
  where
    report = traverse_ $ hPrint stderr
    assemble dense objs = do
      let assembler = if dense then assembleDensely else assembleVerbosely
      putStr =<< assembler objs
