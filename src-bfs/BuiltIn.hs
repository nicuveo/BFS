module BuiltIn
  ( preludeFile
  , builtinFunctions
  ) where

import Data.Bits
import Data.Char
import Data.HashMap.Strict qualified as M
import Data.List           qualified as L

import Grammar
import Location
import Object

import Paths_BFS

--------------------------------------------------------------------------------
-- available builtins

preludeFile :: IO String
preludeFile = readFile =<< getDataFileName "src/Prelude.bs"

builtinFunctions :: ObjectMap
builtinFunctions = M.fromList do
  f <- [pushc, pushb, pushi, set, inc, dec, prints] ++ dupFunctions ++ rollFunctions
  pure (funcName f, builtinLocation $ FunctionObject f)

--------------------------------------------------------------------------------
-- core implementation

pushc :: Function
pushc = Function "pushc" Impure Inline [(BFChar, "c")] [] [BFChar] \case
  [("c", VChar c)] ->
    pure $ builtinLocation $ RawBrainfuck $ ">[-]" ++ replicate (ord c) '+'
  _ ->
    error "ICE: built-in function pushc wasn't properly typechecked"

pushb :: Function
pushb = Function "pushb" Impure Inline [(BFBool, "b")] [] [BFBool] \case
  [("b", VBool b)] ->
    pure $ builtinLocation $ RawBrainfuck $ ">[-]" ++ if b then "+" else ""
  _ ->
    error "ICE: built-in function pushb wasn't properly typechecked"

pushi :: Function
pushi = Function "pushi" Impure Inline [(BFInt, "x")] [] [BFInt] \case
  [("x", VInt x)] ->
    pure $ builtinLocation $ RawBrainfuck $ concat [">[-]" ++ replicate i '+' | i <- decompose x]
  _ ->
    error "ICE: built-in function pushi wasn't properly typechecked"
  where
    decompose x = [ mod (div x 16777216) 256 .|. (if x < 0 then 128 else 0)
                  , mod (div x    65536) 256
                  , mod (div x      256) 256
                  , mod      x           256
                  ]

set :: Function
set = Function "set" Impure Inline [(BFChar, "x")] [BFChar] [BFChar] set_
  where set_ [("x", VChar x)] = [builtinLocation $ RawBrainfuck $ "[-]" ++ replicate (ord x) '+']
        set_ _ = error "ICE"

inc :: Function
inc = Function "inc" Impure Inline [(BFChar, "x")] [BFChar] [BFChar] inc_
  where inc_ [("x", VChar x)] = [builtinLocation $ RawBrainfuck $ replicate (ord x) '+']
        inc_ _ = error "ICE"

dec :: Function
dec = Function "dec" Impure Inline [(BFChar, "x")] [BFChar] [BFChar] dec_
  where dec_ [("x", VChar x)] = [builtinLocation $ RawBrainfuck $ replicate (ord x) '-']
        dec_ _ = error "ICE"


prints :: Function
prints = Function "prints" Impure Block [(BFString, "s")] [] [] dec_
  where dec_ [("s", VString s)] = builtinLocation . RawBrainfuck <$>
          [ ">[-]"
          , snd $ L.foldl' nextChar (0, "") $ ord <$> s
          , "[-]<"
          ]
        dec_ _ = error "ICE"
        nextChar (p, output) n
          | p < n     = (n, output ++ replicate (n - p) '+' ++ ".")
          | otherwise = (n, output ++ replicate (p - n) '-' ++ ".")


rollFunctions :: [Function]
rollFunctions   = rollcn : rollin : concat [[rollc n, rolli n] | n <- [2 .. 9]]
  where rollcn  = Function "rollcn" Impure Inline [(BFInt, "n"), (BFInt, "s")] [] [] $ rolln 1
        rollin  = Function "rollin" Impure Inline [(BFInt, "n"), (BFInt, "s")] [] [] $ rolln 4
        rollc n = Function ("rollc" ++ show n) Impure Inline [(BFInt, "s")] (replicate n BFChar) (replicate n BFChar) $ roll 1 n
        rolli n = Function ("rolli" ++ show n) Impure Inline [(BFInt, "s")] (replicate n BFInt ) (replicate n BFInt ) $ roll 4 n
        rolln k [("n", VInt n), ("s", VInt s)] = rollCode (n*k) $ s*k
        rolln _ _                              = error "ICE"
        roll  k n [("s", VInt s)] = rollCode (n*k) $ s*k
        roll  _ _ _               = error "ICE"

rollCode :: Int -> Int -> [WithLocation Instruction]
rollCode n s = builtinLocation . RawBrainfuck <$> [ ">[-]" ++ replicate s '+'
                                                  , "[-<[->>+<<]"
                                                  , concat $ replicate (n-1) "<[->+<]"
                                                  , replicate n '>'
                                                  , ">[-"
                                                  , replicate n '<'
                                                  , "<+>"
                                                  , replicate n '>'
                                                  , "]<]<"
                                                  ]


dupFunctions :: [Function]
dupFunctions   = dupcn : dupin : concat [[dupc n, dupi n] | n <- [1 .. 9]]
  where dupcn  = Function "dupcn" Impure Inline [(BFInt, "n")] [] [] $ dupn 1
        dupin  = Function "dupin" Impure Inline [(BFInt, "n")] [] [] $ dupn 4
        dupc n = Function ("dupc" ++ show n) Impure Inline [] (replicate n BFChar) (replicate (2*n) BFChar) $ dup n
        dupi n = Function ("dupi" ++ show n) Impure Inline [] (replicate n BFInt ) (replicate (2*n) BFInt ) $ dup $ 4 * n
        dupn k [("n", VInt n)] = dupCode $ n * k
        dupn _ _               = error "ICE"
        dup  n [] = dupCode n
        dup  _ _  = error "ICE"

dupCode :: Int -> [WithLocation Instruction]
dupCode n = fmap (builtinLocation . RawBrainfuck) $ concat $
  replicate n [ replicate (n-1) '<', "[-"
              , replicate n '>', "+>+<"
              , replicate n '<', "]>"
              , replicate n '>', "[-"
              , replicate n '<', "<+>"
              , replicate n '>', "]<"
              ]

--------------------------------------------------------------------------------
-- local helpers

builtinLocation :: a -> WithLocation a
builtinLocation = WL BuiltIn
