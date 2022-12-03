{-|

This optimizer could be a standalone program, as it does not depend on
our pseudo-forth made-up language. `optimize` simply takes a string,
parses it, and performs a small number of simple optimizations.

* dead code is removed: a loop after a loop will never get executed,
  and can be safely removed
* modification before zeroing a cell is ultimately useless and can
  be discarded
* segments of movement and increase / decrease can be combined and
  reordered, for simplification
* all code after the last input / output is dead code

-}

module Optimizer (optimize) where

import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.HashMap.Strict qualified as M
import Data.List           (dropWhileEnd, sortOn)
import Data.Maybe
import Data.Traversable
import Text.Parsec         hiding (State)
import Text.Printf

import Misc

--------------------------------------------------------------------------------
-- api

optimize :: String -> String
optimize = render . cleanEnd . cleanStart . simplify . merge . decompose

--------------------------------------------------------------------------------
-- internal representation

data Code
  = Input
  | Output
  | Loop [Code]
  | Assignment Int (M.HashMap Int Value)
  deriving (Eq)

data Value
  = Relative Int
  | Absolute Int
  deriving (Eq, Ord)

makeRelative :: Value -> Value
makeRelative = \case
  Absolute x -> Relative x
  Relative x -> Relative x

--------------------------------------------------------------------------------
-- parsing the code

decompose :: String -> [Code]
decompose = either (error . show) id . parse code "<optimizer>" . filter (`elem` brainfuckChars)
  where
    code = many $ choice
      [ Input  <$ char ','
      , Output <$ char '.'
      , try $ zero <$ string "[-]"
      , char '[' *> (Loop <$> code) <* char ']'
      , interpret <$> many1 (oneOf "<>+-")
      ]
    zero = Assignment 0 $ M.singleton 0 (Absolute 0)
    interpret s = let (e, a) = foldl' step (0, M.empty) s
                  in  Assignment e $ M.map Relative a
    step (c, m) '>' = (c+1,m)
    step (c, m) '<' = (c-1,m)
    step (c, m) '+' = (c,M.insertWith (+) c   1  m)
    step (c, m) '-' = (c,M.insertWith (+) c (-1) m)
    step _ _        = error "optimizer internal error"

--------------------------------------------------------------------------------
-- optimization

merge :: [Code] -> [Code]
merge = \case
  Assignment e a : Assignment f b : c ->
    merge $ Assignment (e + f) (M.unionWith combine a $ translate e b) : c
  Loop [Loop l] : c -> merge $ Loop l : c
  Loop l : c -> Loop (merge l) : merge c
  a : b -> a : merge b
  [] -> []

combine :: Value -> Value -> Value
combine (Relative x) (Relative y) = Relative (x + y)
combine (Absolute x) (Relative y) = Absolute (x + y)
combine _            (Absolute y) = Absolute y

translate :: Int -> M.HashMap Int a -> M.HashMap Int a
translate n = M.mapKeys (+n)

type LocalState = M.HashMap Int Int

getValue :: MonadState LocalState m => Int -> m (Maybe Int)
getValue = gets . M.lookup

setValue :: MonadState LocalState m => Int -> Int -> m ()
setValue = modify ... M.insert

deleteValue :: MonadState LocalState m => Int -> m ()
deleteValue = modify . M.delete

translateValues :: MonadState LocalState m => Int -> m ()
translateValues = modify . translate

clearValues :: MonadState LocalState m => m ()
clearValues = put mempty

restrict :: Int -> Int
restrict = (`mod` 256)

simplify :: [Code] -> [Code]
simplify = flip evalState mempty . fmap catMaybes . traverse step
  where
    step = \case
      Output ->
        pure $ Just Output
      Input  -> do
        deleteValue 0
        pure $ Just Input
      Assignment e a -> do
        a' <- for (M.toList a) \(index, diff) -> do
          current <- getValue index
          case (current, diff) of
            (Nothing, Relative (restrict -> x)) ->
              pure $ Just (index, Relative $ if x > 128 then x-256 else x)
            (Nothing, Absolute (restrict -> x)) -> do
              setValue index x
              pure $ Just (index, Absolute $ if x > 128 then x-256 else x)
            (Just x, Relative y) -> do
              let target = restrict $ x + y
              setValue index target
              pure $ Just (index, compute x target)
            (Just x, Absolute (restrict -> y)) ->
              if x == y
              then pure Nothing
              else do
                setValue index y
                pure $ Just (index, compute x y)
        translateValues (-e)
        pure $ Just $ Assignment e $ M.fromList $ catMaybes a'
      Loop l -> do
        getValue 0 >>= \case
          Just 0 -> pure Nothing
          _ -> do
            let body = simplify l
            clearValues
            setValue 0 0
            pure $ Just $ Loop body
    smallestDistance = minimumBy (compare `on` abs)
    compute start goal =
      let delta = smallestDistance [goal - start, goal - start + 256, goal - start - 256]
          target = smallestDistance [goal, goal - 256]
      in if abs delta > abs target + 3 then Absolute target else Relative delta

cleanStart :: [Code] -> [Code]
cleanStart = \case
  -- no need to set values to 0 at the start of the program
  Assignment e a : c -> Assignment e (M.map makeRelative a) : c
  c                  -> c

cleanEnd :: [Code] -> [Code]
cleanEnd = dropWhileEnd canBeRemoved
  where
    -- anything after the last input / output is "useless", in the sense that it
    -- only modifies the memory of the program, and can therefore be removed.
    -- warning, however: this is an intentional change in behaviour; if a
    -- program contains an intentional infinite loop at the end, this loop might
    -- be removed
    canBeRemoved = \case
      Input    -> False
      Output   -> False
      (Loop l) -> all canBeRemoved l
      _        -> True

--------------------------------------------------------------------------------
-- render

render :: [Code] -> String
render = foldMap \case
  Input  -> ","
  Output -> "."
  Loop c -> printf "[%s]" $ render c
  Assignment e a ->
    let targets = M.toList a & if e < 0
          then sortOn (negate . fst)
          else sortOn fst
        (res, i) = foldl' step ("", 0) targets
    in res ++ move i e
  where
    step (str, from) (to, v) = (str ++ move from to ++ change v, to)
    move x y
      | y >= x    = replicate (y-x) '>'
      | otherwise = replicate (x-y) '<'
    change (Absolute x) = "[-]" ++ change (Relative x)
    change (Relative x)
      | x >= 0    = replicate   x  '+'
      | otherwise = replicate (-x) '-'
