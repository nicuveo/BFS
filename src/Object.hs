module Object
  ( Object (..)
  , ObjectMap
  ) where

import Data.HashMap.Strict qualified as M

import Grammar
import Location

data Object
  = FunctionObject Function
  | ValueObject Value
  deriving (Show)

type ObjectMap = M.HashMap String (WithLocation Object)
