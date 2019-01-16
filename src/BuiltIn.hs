module BuiltIn where



-- imports

import           Data.Bits
import           Data.Char
import           Data.List as L
import qualified Data.Map  as M

import           Grammar
import           Module
import           Object

import           Paths_BFS



-- prelude

preludeFile :: IO String
preludeFile = readFile =<< getDataFileName "src/Prelude.bs"



-- builtin functions

builtinFunctions :: ObjectMap
builtinFunctions = foldl' insertFunc M.empty $ [pushc, pushi, set, inc, dec, prints] ++ dupFunctions ++ rollFunctions
  where insertFunc m f = M.insert (funcName f) (builtinLocation $ FunctionObject f) m


pushc :: Function
pushc = Function "pushc" False True [(BFChar, "c")] [] [BFChar] pushc_
  where pushc_ [("c", VChar c)] = [builtinLocation $ RawBrainfuck $ ">[-]" ++ replicate (ord c) '+']
        pushc_ _ = error "ICE"

pushi :: Function
pushi = Function "pushi" False True [(BFInt, "x")] [] [BFInt] pushi_
  where pushi_ [("x", VInt x)] = [builtinLocation $ RawBrainfuck $ concat [">[-]" ++ replicate i '+' | i <- decompose x]]
        pushi_ _ = error "ICE"
        decompose x = [ mod (div x 16777216) 256 .|. (if x < 0 then 128 else 0)
                      , mod (div x    65536) 256
                      , mod (div x      256) 256
                      , mod      x           256
                      ]

set :: Function
set = Function "set" False True [(BFChar, "x")] [BFChar] [BFChar] set_
  where set_ [("x", VChar x)] = [builtinLocation $ RawBrainfuck $ "[-]" ++ replicate (ord x) '+']
        set_ _ = error "ICE"

inc :: Function
inc = Function "inc" False True [(BFChar, "x")] [BFChar] [BFChar] inc_
  where inc_ [("x", VChar x)] = [builtinLocation $ RawBrainfuck $ replicate (ord x) '+']
        inc_ _ = error "ICE"

dec :: Function
dec = Function "dec" False True [(BFChar, "x")] [BFChar] [BFChar] dec_
  where dec_ [("x", VChar x)] = [builtinLocation $ RawBrainfuck $ replicate (ord x) '-']
        dec_ _ = error "ICE"


prints :: Function
prints = Function "prints" False False [(BFString, "s")] [] [] dec_
  where dec_ [("s", VString s)] = builtinLocation . RawBrainfuck <$>
          [ ">[-]"
          , snd $ foldl' nextChar (0, "") $ ord <$> s
          , "<"
          ]
        dec_ _ = error "ICE"
        nextChar (p, output) n
          | p < n     = (n, output ++ replicate (n - p) '+' ++ ".")
          | otherwise = (n, output ++ replicate (p - n) '-' ++ ".")


rollFunctions :: [Function]
rollFunctions   = rollcn : rollin : concat [[rollc n, rolli n] | n <- [2 .. 9]]
  where rollcn  = Function "rollcn" False True [(BFInt, "n"), (BFInt, "s")] [] [] $ rolln 1
        rollin  = Function "rollin" False True [(BFInt, "n"), (BFInt, "s")] [] [] $ rolln 4
        rollc n = Function ("rollc" ++ show n) False True [(BFInt, "s")] (replicate n BFChar) (replicate n BFChar) $ roll n
        rolli n = Function ("rolli" ++ show n) False True [(BFInt, "s")] (replicate n BFInt ) (replicate n BFInt ) $ roll $ 4 * n
        rolln k [("n", VInt n), ("s", VInt s)] = rollCode (n*k) s
        rolln _ _ = error "ICE"
        roll  n [("s", VInt s)] = rollCode n s
        roll  _ _ = error "ICE"

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
  where dupcn  = Function "dupcn" False True [(BFInt, "n")] [] [] $ dupn 1
        dupin  = Function "dupin" False True [(BFInt, "n")] [] [] $ dupn 4
        dupc n = Function ("dupc" ++ show n) False True [] (replicate n BFChar) (replicate (2*n) BFChar) $ dup n
        dupi n = Function ("dupi" ++ show n) False True [] (replicate n BFInt ) (replicate (2*n) BFInt ) $ dup $ 4 * n
        dupn k [("n", VInt n)] = dupCode $ n * k
        dupn _ _ = error "ICE"
        dup  n [] = dupCode n
        dup  _ _ = error "ICE"

dupCode :: Int -> [WithLocation Instruction]
dupCode n = fmap (builtinLocation . RawBrainfuck) $ concat $
  replicate n [ replicate (n-1) '<', "[-"
              , replicate n '>', "+>+<"
              , replicate n '<', "]>"
              , replicate n '>', "[-"
              , replicate n '<', "<+>"
              , replicate n '>', "]<"
              ]


builtinLocation :: a -> WithLocation a
builtinLocation = WL BuiltIn
