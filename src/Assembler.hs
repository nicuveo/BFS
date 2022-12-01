{-# LANGUAGE ParallelListComp #-}

module Assembler where

import           Control.Monad
import           Data.Function
import           Data.List       as L
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M
import           Text.Printf

import           Compiler
import           Grammar
import           Misc
import           Module
import           Object
import           Optimizer


assembleVerbosely :: MonadFail m => ObjectMap -> m String
assembleVerbosely objs = unlines <$> expandFunc 0 mainFunc []
  where mainFunc = extractFun objs "main"
        postProd = (chunkify =<<) . groupBy bothCode . lines . concat
        bothCode = (&&) `on` (not . isPrefixOf " ")
        chunkify s
          | " " `isPrefixOf` concat s = s
          | otherwise = chunksOf 80 $ concat s
        indent   = map ("  " ++)
        render v = filter (`notElem` "+-[],.<>") $ intercalate "; " [n ++ "=" ++ show x | (n,x) <- v]
        expandFunc :: MonadFail m => Int -> Function -> [(String, Value)] -> m [String]
        expandFunc level func args = do
          when (level > 99) $ fail "stack too deep"
          i <- postProd <$> mapM expandInst (funcBody func args)
          let header = printf "%s%s" (funcName func) $ if null args
                                                       then ""
                                                       else "(" ++ render args ++ ")"
          return $ if funcInline func
                   then i
                   else header : i
          where expandInst :: MonadFail m => WithLocation Instruction -> m String
                expandInst (WL _ (RawBrainfuck r)) = return r
                expandInst (WL _ (FunctionCall g v)) = do
                  let callee = extractFun objs g
                  result <- expandFunc (level+1) callee $ extractParams objs func args callee v
                  return $ if funcInline callee
                           then dropWhileEnd (== '\n') $ unlines result
                           else unlines $ "" : indent result
                expandInst (WL _ (Loop b)) = do
                  i <- indent . postProd <$> mapM expandInst b
                  return $ printf "[\n  loop\n%s]\n" $ dropWhileEnd(=='\n') $ unlines i
                expandInst (WL _ (While c b)) = do
                  t <- mapM expandInst c
                  i <- mapM expandInst b
                  let ppt = dropWhileEnd(=='\n') $ unlines $ postProd t
                      ppi = dropWhileEnd(=='\n') $ unlines $ indent $ postProd $ i ++ t
                  return $ printf "%s[[-]<\n  while\n%s]<\n" ppt ppi
                expandInst (WL _ (If c b)) = do
                  t <- dropWhileEnd(=='\n') . unlines .          postProd <$> mapM expandInst c
                  i <- dropWhileEnd(=='\n') . unlines . indent . postProd <$> mapM expandInst b
                  return $ printf "%s[[-]<\n  if\n%s\n>[-]]<\n" t i

assembleDensely :: MonadFail m => ObjectMap -> m String
assembleDensely objs = unlines . chunksOf 120 . optimize . concat <$> expandFunc 0 mainFunc []
  where mainFunc = extractFun objs "main"
        expandFunc :: MonadFail m => Int -> Function -> [(String, Value)] -> m [String]
        expandFunc level func args = do
          when (level > 99) $ fail "stack too deep"
          mapM expandInst $ funcBody func args
          where expandInst :: MonadFail m => WithLocation Instruction -> m String
                expandInst (WL _ (RawBrainfuck r)) = return r
                expandInst (WL _ (FunctionCall g v)) = do
                  let callee = extractFun objs g
                  concat <$> expandFunc (level+1) callee (extractParams objs func args callee v)
                expandInst (WL _ (Loop b)) = do
                  i <- concat <$> mapM expandInst b
                  return $ printf "[%s]" i
                expandInst (WL _ (While c b)) = do
                  t <- concat <$> mapM expandInst c
                  i <- concat <$> mapM expandInst b
                  return $ printf "%s[[-]<%s%s]<" t i t
                expandInst (WL _ (If c b)) = do
                  t <- concat <$> mapM expandInst c
                  i <- concat <$> mapM expandInst b
                  return $ printf "%s[[-]<%s>[-]]<" t i


extractParams :: ObjectMap -> Function -> [(String, Value)] -> Function -> [Expression] -> [(String, Value)]
extractParams objs _ argsCaller callee argsCallee =
  [ (name, either (const $ error "FIXME") id $ eval scope kind expr)
  | (kind, name) <- funcArgs callee
  | expr         <- argsCallee
  ]
  where scope = M.union (M.fromList $ fmap (WL undefined . ValueObject) <$> argsCaller) objs

extractFun :: ObjectMap -> String -> Function
extractFun = getFun ... (M.!)
  where getFun (WL _ (FunctionObject f)) = f
        getFun _                         = error "should never happen"
