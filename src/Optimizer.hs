{- Optimizer.hs

This optimizer could be a standalone program, as it does not depend on
our pseudo-forth made-up language. `optimize` simply takes a string,
parses it, and performs a small number of simple optimizations.

* dead code is removed: a loop after a loop will never get executed,
  and can be safely removed
* modifification before zeroing a cell is ultimately useless and can
  be discarded
* segments of movement and increase / decrease can be combined and
  reordered, for simplification
* all code after the last input / output is dead code

-}

module Optimizer (optimize) where



-- imports

import           Data.Function
import           Data.List     (dropWhileEnd, foldl', minimumBy)
import qualified Data.Map      as M
import           Text.Parsec
import           Text.Printf



-- optimizer

optimize :: String -> String
optimize = show . simplify . decompose



-- internal representation

data Value = Relative Int
           | Absolute Int
           deriving (Eq, Ord)

data Code = Zero
          | Input
          | Output
          | Loop [Code]
          | Assignment Int (M.Map Int Value)
          deriving Eq


instance Show Code where
  showList c = ((c >>= show) ++)
  show Zero             = "[-]"
  show Input            = ","
  show Output           = "."
  show (Loop c)         = printf "[%s]" $ show c
  show (Assignment e a) = res ++ move pos e
    where (res, pos) = shortest $ M.filter (/= Relative 0) a
          step (s, p) (np,c) = (s ++ move p np ++ change c, np)
          move x y
            | y >= x    = replicate (y-x) '>'
            | otherwise = replicate (x-y) '<'
          change (Absolute x) = "[-]" ++ change (Relative x)
          change (Relative x)
            | x >= 0    = replicate   x  '+'
            | otherwise = replicate (-x) '-'
          shortest s    = minimumBy (compare `on` length . fst) $ foldl' step ("", 0) <$> [M.toAscList s, M.toDescList s]


combine :: Value -> Value -> Value
combine _            (Absolute x) = Absolute x
combine (Relative x) (Relative d) = Relative $ x + d
combine (Absolute x) (Relative d) = Absolute $ x + d

isIO :: Code -> Bool
isIO Input    = True
isIO Output   = True
isIO (Loop l) = any isIO l
isIO _        = False



-- parsing the code

decompose :: String -> [Code]
decompose = either (error . show) id . parse code "<optimizer>" . filter (`elem` "<>+-.,[]")
  where code = many $ choice [ Input  <$ char ','
                             , Output <$ char '.'
                             , try $ Zero <$ string "[-]"
                             , char '[' *> (Loop <$> code) <* char ']'
                             , interpret <$> many1 (oneOf "<>+-")
                             ]
        interpret s = let (e, a) = foldl' step (0, M.empty) s
                      in  Assignment e $ M.map Relative a
        step (c, m) '>' = (c+1,m)
        step (c, m) '<' = (c-1,m)
        step (c, m) '+' = (c,M.insertWith (+) c   1  m)
        step (c, m) '-' = (c,M.insertWith (+) c (-1) m)
        step _ _ = error "optimizer internal error"

simplify :: [Code] -> [Code]
simplify = clean . dropWhileEnd (not . isIO) . simplify_
  where -- two loops in a row: the second one is dead code
        simplify_ (Zero           : Zero           : c) = simplify_ $ Zero   : c
        simplify_ (Zero           : Loop _         : c) = simplify_ $ Zero   : c
        simplify_ (Loop l         : Zero           : c) = simplify_ $ Loop l : c
        simplify_ (Loop l         : Loop _         : c) = simplify_ $ Loop l : c
        -- simple loop withing a loop: the outer layer can be removed
        simplify_ (Loop [Loop l]                   : c) = simplify_ $ Loop l : c
        simplify_ (Loop [Zero]                     : c) = simplify_ $ Zero   : c
        simplify_ (Loop l                          : c) = Loop (simplify_ l) : simplify_ c
        -- assignment and zeros can be combined
        simplify_ (Assignment e a : Zero           : c) = simplify_ $ Assignment e (M.insert e (Absolute 0) a) : c
        simplify_ (Zero           : Assignment e a : c) = simplify_ $ Assignment e (M.insertWith combine 0 (Absolute 0) a) : c
        simplify_ (Assignment e a : Assignment f b : c) = simplify_ $ Assignment (e+f) (M.foldlWithKey' (\m k x -> M.insertWith (flip combine) (e+k) x m) a b) : c
        -- otherwise simple recursion
        simplify_ (e:c)                                 = e : simplify_ c
        simplify_ []                                    = []
        -- no need to set values to 0 at the start of the program
        clean (Assignment e a : c) = Assignment e (M.map toRelative a) : c
        clean c                    = c
        toRelative (Absolute x) = Relative x
        toRelative v            = v
