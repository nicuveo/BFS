module Assembler
  ( assembleVerbosely
  , assembleDensely
  ) where

import Control.Monad
import Data.Function
import Data.HashMap.Strict qualified as M
import Data.List           qualified as L
import Data.List.Split     (chunksOf)
import Text.Printf

import Eval
import Grammar
import Location
import Misc
import Object
import Optimizer

assembleVerbosely :: MonadFail m => ObjectMap -> m String
assembleVerbosely objs =
  unlines . concat . map snd <$> translate objs go
  where
    go inst cond body = case inst of
      RawBrainfuck r ->
        pure $ pure (True, [r])
      Loop _ ->
        pure $ pure ( False
                    , concat [ ["do ["]
                             , indent $ merge body
                             , ["done ]"]
                             ]
                    )
      While _ _ ->
        pure $ pure ( False
                    , concat [ ["while"]
                             , indent $ merge cond
                             , ["do [[-]<"]
                             , indent $ merge $ body ++ cond
                             , ["done ]<"]
                             ]
                    )
      If _ _ ->
        pure $ pure ( False
                    , concat [ ["if"]
                             , indent $ merge cond
                             , ["then [[-]<"]
                             , indent $ merge body
                             , ["end >[-]]<"]
                             ]
                    )
      FunctionCall name args -> do
        target <- retrieveFunction objs name
        pure $ case funcRender target of
          Inline -> concat body
          Block  ->
            pure ( False
                 , concat [ [name ++ render (zip (funcArgs target) args) ++ " {"]
                          , indent $ merge body
                          , ["}"]
                          ]
                 )
    merge bundle = concat $ map reduce $ L.groupBy ((&&) `on` fst) $ concat bundle
    indent = map ("  " ++)
    reduce = \case
      [(False, x)] -> x
      l            -> chunksOf 60 $ concat $ concat $ map snd l
    render [] = ""
    render a = printf "(%s)" $ unwords do
      ((_type, name), expr) <- a
      pure $ filter (`notElem` brainfuckChars) $ name ++ case expr of
        LiteralInt i -> "=" ++ show i
        LiteralChar c -> "=" ++ show c
        LiteralString s -> "=" ++ show s
        ConstantName n
          | Just (WL _ (ValueObject v)) <- M.lookup n objs -> "=" ++ show v
          | otherwise -> ""

assembleDensely :: MonadFail m => ObjectMap -> m String
assembleDensely objs = unlines . chunksOf 100 . optimize <$> translate objs go
  where
    go inst (concat -> args) (concat -> body) = case inst of
      RawBrainfuck r   -> pure r
      FunctionCall _ _ -> pure $ body
      Loop _           -> pure $ printf "[%s]" body
      While _ _        -> pure $ printf "%s[[-]<%s%s]<" args body args
      If _ _           -> pure $ printf "%s[[-]<%s>[-]]<" args body

--------------------------------------------------------------------------------
-- helpers

translate
  :: MonadFail m
  => ObjectMap
  -> (Instruction -> [a] -> [a] -> m a)
  -> m a
translate objs f = do
  main <- retrieveFunction objs "main"
  fnbody <- traverse (go []) (funcBody main [])
  f (FunctionCall "main" []) [] fnbody
  where
    go localScope = step localScope . getEntry
    step localScope inst = case inst of
      RawBrainfuck _ ->
        f inst [] []
      Loop body -> do
        b <- traverse (go localScope) body
        f inst [] b
      While cond body -> do
        c <- traverse (go localScope) cond
        b <- traverse (go localScope) body
        f inst c b
      If cond body -> do
        c <- traverse (go localScope) cond
        b <- traverse (go localScope) body
        f inst c b
      FunctionCall name providedArgs -> do
        target <- retrieveFunction objs name
        let scope = M.union objs $ M.fromList $
              (fmap . fmap) (WL FunctionArgument . ValueObject) localScope
            expectedArgs = funcArgs target
        params <- zipWithM (evalParam scope) providedArgs expectedArgs
        fnbody <- traverse (go params) $ funcBody target params
        f inst [] fnbody

evalParam :: MonadFail m => ObjectMap -> Expression -> Variable -> m (String, Value)
evalParam objs expr (argType, argName) = do
  value <- eval objs argType expr `onLeft` \e -> fail (show e)
  pure (argName, value)

retrieveFunction :: MonadFail m => ObjectMap -> String -> m Function
retrieveFunction objs name = M.lookup name objs & \case
  Just (WL _ (FunctionObject f)) -> pure f
  _ -> fail $ "link error: function " ++ name ++ " not found!"
