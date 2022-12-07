module Eval
  ( eval
  , canCastTo
  ) where

import Data.Char
import Data.HashMap.Strict qualified as M

import Diagnostics
import Grammar
import Location
import Object

eval :: ObjectMap -> Type -> Expression -> Either Error Value
eval objs kind = \case
  LiteralString s -> case kind of
    BFString -> Right $ VString s
    _        -> Left  $ StringLiteralError kind s
  LiteralChar c -> case kind of
    BFChar -> Right $ VChar c
    BFBool -> Right $ VBool $ ord c /= 0
    _      -> Left  $ CharLiteralError kind c
  LiteralBool b -> case kind of
    BFBool -> Right $ VBool b
    _      -> Left  $ BoolLiteralError kind b
  LiteralInt i -> case kind of
    BFChar   -> if i < 0 || i > 255
                then Left  $ IntLiteralError kind i
                else Right $ VChar $ chr i
    BFString -> Left  $ IntLiteralError kind i
    BFBool   -> Right $ VBool $ i /= 0
    BFInt    -> Right $ VInt i
  ConstantName n
    | not $ n `M.member` objs -> Left $ ConstantNotFoundError n
    | otherwise -> case objs M.! n of
        WL _ (ValueObject val) -> cast val kind
        wp                     -> Left $ ExpectedValueGotFunctionError n wp

canCastTo :: Type -> Type -> Bool
BFChar `canCastTo` BFInt  = True
BFChar `canCastTo` BFBool = True
BFInt  `canCastTo` BFBool = True
a      `canCastTo` b      = a == b

cast :: Value -> Type -> Either Error Value
cast (VChar c) BFInt  = Right $ VInt  $ ord c
cast (VChar c) BFBool = Right $ VBool $ ord c /= 0
cast (VInt  i) BFBool = Right $ VBool $ i /= 0
cast value kind
  | typeof value == kind = Right value
  | otherwise            = Left $ ImplicitCastError kind $ typeof value
