module Location
  ( Location (..)
  , WithLocation (..)
  ) where

import Text.Printf

data Location
  = SourceFile
  { locSource :: String
  , locLine   :: Int
  , locColumn :: Int
  }
  | CommandLineArgument
  | FunctionArgument
  | BuiltIn

data WithLocation a = WL
  { getLocation :: Location
  , getEntry    :: a
  }
  deriving (Functor)

instance Show Location where
  show CommandLineArgument = "command-line argument"
  show FunctionArgument    = "function argument"
  show BuiltIn             = "<built-in function>"
  show (SourceFile s l c)  = printf "%s:%d:%d" s l c

instance Show a => Show (WithLocation a) where
  show (WL loc x) = printf "%s: %s" (show loc) $ show x
