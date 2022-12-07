{-|

This file exposes the AST of our made-up language and a few helper
functions. Elements are stored with their location, for diagnostic purposes.

-}

module Grammar
  ( Collection
  , Program
  , Statement (..)
  , Expression (..)
  , Instruction (..)
  , Constant (..)
  , Purity (..)
  , Render (..)
  , Function (..)
  , Type (..)
  , Variable
  , Value (..)
  , isImpure
  , anyImpure
  , typeof
  ) where

import Data.List   qualified as L
import Text.Printf

import Location

--------------------------------------------------------------------------------
-- ast

type Collection a = [WithLocation a]

type Program = Collection Statement

data Statement
  = Include String
  | ConstantDecl Constant
  | FunctionDecl Function
  deriving (Show)

data Instruction
  = FunctionCall String [Expression]
  | RawBrainfuck String
  | If (Collection Instruction) (Collection Instruction)
  | Loop (Collection Instruction)
  | While (Collection Instruction) (Collection Instruction)
  deriving (Show)

data Expression
  = ConstantName String
  | LiteralString String
  | LiteralChar Char
  | LiteralBool Bool
  | LiteralInt Int
  deriving (Show)

data Constant = Constant
  { constType :: Type
  , constName :: String
  , constExpr :: Expression
  }
  deriving (Show)

data Purity
  = Pure
  | Impure
  deriving (Eq, Show)

data Render
  = Inline
  | Block
  deriving (Eq, Show)

data Function = Function
  { funcName   :: String
  , funcPurity :: Purity
  , funcRender :: Render
  , funcArgs   :: [Variable]
  , funcInput  :: [Type]
  , funcOutput :: [Type]
  , funcBody   :: [(String, Value)] -> Collection Instruction
  }

data Type
  = BFInt
  | BFChar
  | BFString
  | BFBool
  deriving (Eq)

data Value
  = VInt Int
  | VChar Char
  | VString String
  | VBool Bool
  deriving (Eq)

type Variable  = (Type, String)

--------------------------------------------------------------------------------
-- instances

instance Show Function where
  show f = printf "{%sfunction %s(%s) %s -> %s}" q n a i o
    where q = unwords $
               ["impure" | Impure <- pure $ funcPurity f] ++
               ["inline" | Inline <- pure $ funcRender f] ++
               [""]
          n = funcName f
          a = L.intercalate ", " $ show <$> funcArgs f
          i = show $ funcInput f
          o = show $ funcOutput f

instance Show Type where
  show BFInt    = "Int"
  show BFChar   = "Char"
  show BFString = "String"
  show BFBool   = "Bool"

instance Show Value where
  show (VInt    i) = show i
  show (VChar   c) = show c
  show (VString s) = show s
  show (VBool   b) = show b

--------------------------------------------------------------------------------
-- functions

isImpure :: Instruction -> Bool
isImpure (FunctionCall _ _) = False
isImpure (RawBrainfuck s  ) = any (/= '#') s
isImpure (Loop         b  ) = anyImpure b
isImpure (If           c b) = anyImpure c || anyImpure b
isImpure (While        c b) = anyImpure c || anyImpure b

anyImpure :: Collection Instruction -> Bool
anyImpure = any isImpure . map getEntry

typeof :: Value -> Type
typeof (VInt    _) = BFInt
typeof (VChar   _) = BFChar
typeof (VString _) = BFString
typeof (VBool   _) = BFBool
