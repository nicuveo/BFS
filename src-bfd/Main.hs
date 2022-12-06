{- Small debugging program for Brainfuck.

This small programs runs a brainfuck program, and interprets any '#' as a
debugging instruction: if encoutered, a debug line is printed, with the current
status of the tape. This is similar to what beef does, but this small program
always print every byte as an hexadecimal value.
-}

module Main where

import Control.Monad.Loops
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Function
import Data.Int
import Data.Traversable
import Data.Vector.Unboxed.Mutable qualified as V
import Prelude                     hiding (Left, Right)
import System.Environment
import System.IO.Error
import Text.Parsec                 hiding (State)
import Text.Printf


--------------------------------------------------------------------------------
-- Options

-- | Hardcoded tape size.
-- (TODO: make this a runtime parameter)
tapeSize :: Int
tapeSize = 1024


--------------------------------------------------------------------------------
-- Instructions

data Code
  = Input
  | Output
  | Left
  | Right
  | Plus
  | Minus
  | Debug
  | Loop [Code]

parseCode :: String -> [Code]
parseCode = either (error . show) id . parse code "bfd" . filter (`elem` validChars)
  where
    validChars = "[]+-.,<>#" :: String
    code = many $ choice
      [ Input  <$ char ','
      , Output <$ char '.'
      , Debug  <$ char '#'
      , Left   <$ char '<'
      , Right  <$ char '>'
      , Plus   <$ char '+'
      , Minus  <$ char '-'
      , char '[' *> (Loop <$> code) <* char ']'
      ]


--------------------------------------------------------------------------------
-- Execution

type Tape = V.MVector (PrimState IO) Int8

data Position = Position
  { currentPos     :: Int
  , maxEncountered :: Int
  }

newtype BF a
  = BF { runBF :: StateT Position IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState Position)

run :: Tape -> [Code] -> BF ()
run v = traverse_ \case
  Input -> do
    Position i m <- get
    c <- liftIO $ getChar `catchIOError` \e ->
      if isEOFError e then pure $ toEnum 0 else ioError e
    liftIO $ V.unsafeWrite v i $ toEnum $ ord c
    when (i > m) $ put $ Position i i
  Output -> do
    i <- gets currentPos
    c <- liftIO $ V.read v i
    liftIO $ putChar $ chr $ fromEnum c
  Minus -> do
    Position i m <- get
    liftIO $ V.modify v (subtract 1) i
    when (i > m) $ put $ Position i i
  Plus -> do
    Position i m <- get
    liftIO $ V.modify v (+1) i
    when (i > m) $ put $ Position i i
  Left -> do
    Position i m <- get
    let r = mod (i-1) tapeSize
    put $ Position r m
  Right -> do
    Position i m <- get
    let r = mod (i+1) tapeSize
    put $ Position r m
  Loop l -> do
    let nonZero = do
          i <- gets currentPos
          c <- liftIO $ V.read v i
          pure $ c /= 0
    whileM_ nonZero $ run v l
  Debug -> do
    Position i m <- get
    s <- concat <$> for [0..m] \x -> do
      c <- liftIO $ V.unsafeRead v x
      pure $ if x == i then printf "<%2x>" c else printf " %2x " c
    liftIO $ putStrLn $ "[" ++ s ++ "]"


--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  -- FIXME: this doesn't support options, and will crash if no file is given.
  f <- head <$> getArgs
  c <- parseCode <$> readFile f
  v <- V.new tapeSize
  flip evalStateT (Position 0 0) $ runBF $ run v c
