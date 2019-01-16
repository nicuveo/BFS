{- Compiler.hs

The "compiler" has one simple job: turn a file into an object map (see
Module). But given that no real transformation occurs at this point
(function calls are not inlined), what it mostly does is check that
the code is valid and emit diagnostics.

If a function is declared as "impure", then raw brainfuck is allowed,
and types are not checked. Otherwise, a function is checked to make
sure that:
  1. all function calls have the stack in the expected state
  2. the stack state at the end of the function is consistent with
     what was declared.

On top of it, some similar checks are performed for all blocks: the
condition in an if or a while must only add a boolean on top of the
stack, and the body of a block must not alter the stack,

-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler (compile, eval) where



-- imports

import           Control.Monad
import qualified Control.Monad.Except      as E
import qualified Control.Monad.Reader      as R
import qualified Control.Monad.RWS         as A
import qualified Control.Monad.State       as S
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Writer      as W
import           Data.Char
import           Data.List                 as L
import qualified Data.Map                  as M

import           BuiltIn
import           Diagnostics
import           Grammar
import           Misc
import           Module
import           Object
import           Parser



-- exported functions

compile :: Monad m => FileResolver m -> String -> m (Diagnostics, Maybe ObjectMap)
compile fs fn = do
  (result, s, d) <- A.runRWST (E.runExceptT expr) deps st
  case result of
    Right _ -> return (d, Just $ objects s)
    Left  _ -> return (d, Nothing)
  where deps = Dependencies fs
        st   = CompilerState M.empty builtinFunctions
        expr = include (WL undefined "Prelude") >> include (WL CommandLineArgument fn)

eval :: ObjectMap -> Type -> Expression -> Either Error Value
eval _ kind (LiteralString s) = case kind of
  BFString -> Right $ VString s
  _        -> Left  $ StringLiteralError kind s
eval _ kind (LiteralChar c) = case kind of
  BFChar   -> Right $ VChar c
  BFBool   -> Right $ VBool $ ord c /= 0
  _        -> Left  $ CharLiteralError kind c
eval _ kind (LiteralInt  i) = case kind of
  BFString -> Left  $ IntLiteralError kind i
  BFChar   -> if i < 0 || i > 255
              then Left  $ IntLiteralError kind i
              else Right $ VChar $ chr i
  BFBool   -> Right $ VBool $ i /= 0
  BFInt    -> Right $ VInt i
eval objs kind (ConstantName n) =
  if n `M.notMember` objs
  then Left $ ConstantNotFoundError n
  else case objs M.! n of
         WL _ (ValueObject val) -> cast val kind
         wp                     -> Left $ ExpectedValueGotFunctionError n wp



-- compiler monad

type FileResolver m = String -> m (Maybe (String, String))

newtype Dependencies m =
  Dependencies { fileResolver :: FileResolver m
               }

data CompilerState =
  CompilerState { moduleCache :: M.Map String ObjectMap
                , objects     :: ObjectMap
                }

type CompilerT m = E.ExceptT () (A.RWST (Dependencies m) Diagnostics CompilerState m)



-- helpers

addModule :: String -> ObjectMap -> CompilerState -> CompilerState
addModule name object state = state { moduleCache = M.insert name object $ moduleCache state }

addObject :: String -> WithLocation Object -> CompilerState -> CompilerState
addObject name object state = state { objects = M.insert name object $ objects state }

report :: Monad m => WithLocation a -> Error -> CompilerT m ()
report wp e = W.tell [e <$ wp]

reportAndStop :: Monad m => WithLocation a -> Error -> CompilerT m b
reportAndStop = (>> E.throwError ()) ... report

getFunction :: ObjectMap -> String -> Either Error Function
getFunction objs n =
  case objs M.!? n of
    Just (WL _ (FunctionObject g)) -> Right g
    Just wp                        -> Left $ ExpectedFunctionGotValueError n wp
    Nothing                        -> Left $ FunctionNotFoundError n



-- step by step analysis of a program

include :: Monad m => WithLocation String -> CompilerT m ()
include w@(WL _ filename) = do
  (_, diagnostics) <- W.listen $ do
    resolver <- R.asks fileResolver
    state    <- S.get
    cache    <- S.gets moduleCache
    mContent <- lift $ lift $ resolver filename
    case mContent of
      Nothing               -> W.tell [FileNotFoundError filename <$ w]
      Just (mName, content) -> when (mName `M.notMember` cache) $
        case parseProgram mName content of
          Left diagnostic -> W.tell [diagnostic]
          Right program   -> do
            sequence_ $ processStatement <$> program
            S.modify $ addModule mName $ objects state
  when (any isError diagnostics) $ E.throwError ()

processStatement :: Monad m => WithLocation Statement -> CompilerT m ()
processStatement (WL l (Include filename)) = include $ WL l filename
processStatement wp@(WL _ (ConstantDecl n)) = do
  objs <- S.gets objects
  let cName = constName n
  if cName `M.member` objs
  then report wp $ ConstantAlreadyDefinedError cName $ objs M.! cName
  else case eval objs (constType n) $ constExpr n of
    Left  err   -> report wp err
    Right value -> S.modify $ addObject cName $ ValueObject value <$ wp
processStatement wp@(WL _ (FunctionDecl f)) = do
  objs <- S.gets objects
  let fName = funcName f
  if fName `M.member` objs
  then report wp $ FunctionAlreadyDefinedError fName $ objs M.! fName
  else E.catchError (doTheThing fName objs) $ const $ return ()
  where doTheThing fName objs = do
          checkFunction objs $ f <$ wp
          S.modify $ addObject fName $ FunctionObject f <$ wp

checkFunction :: Monad m => ObjectMap -> WithLocation Function -> CompilerT m ()
checkFunction objs wp@(WL _ f) = do
  let argNames = snd <$> funcArgs f
      dupNames = argNames \\ nub argNames
      body     = funcBody f $ error "ICE: tried to compile a built-in function"
  if isPure && anyImpure body
  then report wp $ PureFunctionsContainsImpureCodeError $ funcName f
  else do
    if not $ null dupNames
    then report wp $ DuplicateArgumentNamesError dupNames
    else forM_ (funcArgs f) $ \(_, argName) ->
       when (argName `M.member` objs) $ report wp $ ArgumentNameShadowsObjectWarning argName $ objs M.! argName
    result <- checkInstructions objs f (funcPure f) (reverse $ funcInput f) body
    when (isPure && result /= reverse (funcOutput f)) $ report wp $ FunctionTypeDeclarationError f $ reverse result
  where isPure = funcPure f

checkInstructions :: Monad m => ObjectMap -> Function -> Bool -> [Type] -> [WithLocation Instruction] -> CompilerT m [Type]
checkInstructions objs f typeCheck = foldM $ checkInstruction objs f typeCheck

checkInstruction :: Monad m => ObjectMap -> Function -> Bool -> [Type] -> WithLocation Instruction -> CompilerT m [Type]
checkInstruction _    _ _         stack (WL _ (RawBrainfuck _)) = return stack
checkInstruction objs f typeCheck stack wp@(WL _ (FunctionCall n v)) =
  case getFunction objs n of
    Left err -> reportAndStop wp err
    Right  g ->
      if length (funcArgs g) /= length v
      then report wp (FunctionCallWrongArgumentsNumberError g $ length v) >> return stack
      else do
        forM_ (zip (funcArgs g) v) $ \(arg@(kind, _), expr) -> do
          let thisKind = case expr of
                ConstantName cn -> parameterType kind cn
                _               -> typeof <$> eval objs kind expr
          case thisKind of
            Left err -> report wp err
            Right k  -> when (k /= kind) $ report wp $ FunctionCallWrongArgumentTypeError g arg k
        if typeCheck
        then case stripPrefix (reverse $ funcInput g) stack of
               Nothing -> reportAndStop wp $ FunctionCallStackTypeError g $ reverse stack
               Just ns -> return $ reverse (funcOutput g) ++ ns
        else return stack
  where parameterType k name =
          case find (\(_, arg) -> arg == name) $ funcArgs f of
            Just (kind, _) -> Right $ if kind `canCastTo` k then k else kind
            Nothing        -> case objs M.!? name of
              Just (WL _ (ValueObject   vo)) -> Right $ if typeof vo `canCastTo` k then k else typeof vo
              Just other                     -> Left  $ ExpectedValueGotFunctionError name other
              Nothing                        -> Left  $ ConstantNotFoundError name
checkInstruction objs f typeCheck stack wp@(WL _ (Loop lb)) = do
  newStack <- checkInstructions objs f typeCheck stack lb
  when (typeCheck && newStack /= stack) $ reportAndStop wp $ BlockLoopNotStackNeutralError (stack, newStack)
  return newStack
checkInstruction objs f typeCheck stack wp@(WL _ (If ic ib)) = do
  (cStackIn, cStackOut) <- foldM (guessStack objs f) ([], []) ic
  when (cStackOut /= BFBool : cStackIn) $ report wp $ ConditionWrongTypeError (reverse cStackIn, reverse cStackOut)
  newStack <- checkInstructions objs f typeCheck stack ib
  when (typeCheck && newStack /= stack) $ reportAndStop wp $ BlockIfNotStackNeutralError (stack, newStack)
  return newStack
checkInstruction objs f typeCheck stack wp@(WL _ (While wc wb)) = do
  (cStackIn, cStackOut) <- foldM (guessStack objs f) ([], []) wc
  when (cStackOut /= BFBool : cStackIn) $ report wp $ ConditionWrongTypeError (reverse cStackIn, reverse cStackOut)
  newStack <- checkInstructions objs f typeCheck stack wb
  when (typeCheck && newStack /= stack) $ reportAndStop wp $ BlockWhileNotStackNeutralError (stack, newStack)
  return newStack



-- stack analysis

guessStack :: Monad m => ObjectMap -> Function -> ([Type], [Type]) -> WithLocation Instruction -> CompilerT m ([Type], [Type])
guessStack objs f s@(initStack, currentStack) wp@(WL _ (FunctionCall n _)) =
  case getFunction objs n of
    Left err -> report wp err >> return s
    Right  g -> do
      let missing = drop (length currentStack) $ reverse $ funcInput g
          newInitStack = initStack ++ missing
      newCurrentStack <- checkInstruction objs f True (currentStack ++ missing) wp
      return (newInitStack, newCurrentStack)
guessStack _ _ s wp = report wp (ConditionWrongInstructionError $ getEntry wp) >> return s



-- value casting

cast :: Value -> Type -> Either Error Value
cast (VChar c) BFInt = Right $ VInt $ ord c
cast value kind
  | typeof value == kind = Right value
  | otherwise            = Left $ ImplicitCastError kind $ typeof value

canCastTo :: Type -> Type -> Bool
BFChar `canCastTo` BFInt = True
a      `canCastTo` b     = a == b
