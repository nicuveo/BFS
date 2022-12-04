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

instance Show Object where
  show = \case
    FunctionObject f -> "function: " ++ show f
    ValueObject    v -> "value: "    ++ show v

type ObjectMap = M.HashMap String (WithLocation Object)
