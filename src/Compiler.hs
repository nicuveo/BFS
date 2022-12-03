{-|

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

module Compiler (compile) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Foldable
import Data.Functor         (($>))
import Data.HashMap.Strict  qualified as M
import Data.List            (nub, stripPrefix, (\\))
import Data.These

import BuiltIn
import Diagnostics
import Eval
import Grammar
import Location
import Misc
import Object
import Parser

--------------------------------------------------------------------------------
-- top-level wrapper

compile :: Monad m => FileResolver m -> String -> m (These Diagnostics ObjectMap)
compile resolver filename = runCompiler deps do
  processInclude (WL BuiltIn "Prelude")
  processInclude (WL CommandLineArgument filename)
  where
    deps = Dependencies resolver

--------------------------------------------------------------------------------
-- internal compiler monad

newtype CompilerT m a
  = CompilerT { runCompilerT :: ExceptT () (RWST (Dependencies m) Diagnostics CompilerState m) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError ()
    , MonadFail
    , MonadReader (Dependencies m)
    , MonadState CompilerState
    , MonadWriter Diagnostics
    )

instance MonadTrans CompilerT where
  lift = CompilerT . lift . lift

type FileResolver m = String -> m (Maybe (String, String))

newtype Dependencies m
  = Dependencies { fileResolver :: FileResolver m }

data CompilerState = CompilerState
  { moduleCache :: M.HashMap String ObjectMap
  , objects     :: ObjectMap
  }

runCompiler :: Monad m => Dependencies m -> CompilerT m a -> m (These Diagnostics ObjectMap)
runCompiler deps action = do
  (result, objects -> objs, diagnostics) <- runRWST (runExceptT $ runCompilerT action) deps baseState
  pure $ case (result, diagnostics) of
    (Left _, _)   -> This diagnostics
    (Right _, []) -> That objs
    (Right _, _)  -> These diagnostics objs
  where
    baseState = CompilerState mempty builtinFunctions

--------------------------------------------------------------------------------
-- base operations

registerModule :: Monad m => String -> CompilerT m ()
registerModule name = modify \s -> s { moduleCache = M.insert name (objects s) (moduleCache s) }

registerObject :: Monad m => String -> WithLocation Object -> CompilerT m ()
registerObject name object = modify \s -> s { objects = M.insert name object $ objects s }

report :: Monad m => WithLocation a -> Error -> CompilerT m ()
report wp e = tell [e <$ wp]

fatal :: Monad m => WithLocation a -> Error -> CompilerT m b
fatal wp e = tell [e <$ wp] >> shortCircuit

shortCircuit :: Monad m => CompilerT m a
shortCircuit = throwError ()

retrieveFunction :: Monad m => WithLocation a -> String -> CompilerT m Function
retrieveFunction wl name = do
  objs <- gets objects
  case objs M.!? name of
    Just (WL _ (FunctionObject g)) -> pure g
    Just wp -> fatal wl $ ExpectedFunctionGotValueError name wp
    Nothing -> fatal wl $ FunctionNotFoundError name

--------------------------------------------------------------------------------
-- step by step analysis

processStatement :: Monad m => WithLocation Statement -> CompilerT m ()
processStatement wl = case wl of
  WL l (Include filename) ->
    processInclude $ WL l filename
  WL _ (ConstantDecl n) -> do
    objs <- gets objects
    let cName = constName n
    value <- eval objs (constType n) (constExpr n) `onLeft` \err ->
      fatal wl err
    when (cName `M.member` objs) $
      fatal wl $ ConstantAlreadyDefinedError cName $ objs M.! cName
    registerObject cName $ wl $> ValueObject value
  WL _ (FunctionDecl f) -> do
    objs <- gets objects
    let fName = funcName f
    when (fName `M.member` objs) $
      report wl $ FunctionAlreadyDefinedError fName $ objs M.! fName
    -- we catch any error while processing the function, to return more errors:
    -- at the level module, we re-throw if any non-warning was issued
    processFunction (wl $> f) `catchError` \_ -> pure ()
    registerObject fName $ wl $> FunctionObject f

processInclude :: Monad m => WithLocation String -> CompilerT m ()
processInclude w@(WL _ filename) = do
  (_, diagnostics) <- listen $ do
    resolver <- asks fileResolver
    (name, content) <- lift (resolver filename) `onNothingM`
      fatal w (FileNotFoundError filename)
    cache <- gets moduleCache
    unless (name `M.member` cache) do
      program <- parseProgram name content `onLeft` \e ->
        tell [e] >> shortCircuit
      traverse_ processStatement program
      registerModule name
  when (any isError diagnostics) $
    shortCircuit

processFunction :: Monad m => WithLocation Function -> CompilerT m ()
processFunction wp@(WL _ f) = do
  let argNames = snd <$> funcArgs f
      dupNames = argNames \\ nub argNames
      body     = funcBody f (error "ICE: tried to compile a built-in function")
  when (funcPurity f == Pure && anyImpure body) $
    report wp $ PureFunctionsContainsImpureCodeError $ funcName f
  when (not $ null dupNames) $
    report wp $ DuplicateArgumentNamesError dupNames
  objs <- gets objects
  for_ (funcArgs f) \(_, argName) ->
    when (argName `M.member` objs) $
      report wp $ ArgumentNameShadowsObjectWarning argName $ objs M.! argName
  result <- processInstructions f (funcInput f) body
  when (funcPurity f == Pure && result /= funcOutput f) $
    report wp $ FunctionTypeDeclarationError f $ reverse result

processInstructions :: Monad m => Function -> [Type] -> [WithLocation Instruction] -> CompilerT m [Type]
processInstructions context = foldM $ processInstruction context

processInstruction :: Monad m => Function -> [Type] -> WithLocation Instruction -> CompilerT m [Type]
processInstruction func stack wp@(WL _ instruction) = case instruction of
  RawBrainfuck _ ->
    pure stack
  Loop loopBody -> do
    newStack <- processInstructions func stack loopBody
    when (shouldTypeCheck && newStack /= stack) $
      report wp $ BlockLoopNotStackNeutralError (stack, newStack)
    pure newStack
  While whileCondition whileBody -> do
    (cStackIn, cStackOut) <- foldM (guessStack func) ([], []) whileCondition
    when (cStackOut /= BFBool : cStackIn) $
      report wp $ ConditionWrongTypeError (reverse cStackIn, reverse cStackOut)
    newStack <- processInstructions func stack whileBody
    when (shouldTypeCheck && newStack /= stack) $
      report wp $ BlockWhileNotStackNeutralError (stack, newStack)
    pure newStack
  If ifCondition ifBody -> do
    (cStackIn, cStackOut) <- foldM (guessStack func) ([], []) ifCondition
    when (cStackOut /= BFBool : cStackIn) $
      report wp $ ConditionWrongTypeError (reverse cStackIn, reverse cStackOut)
    newStack <- processInstructions func stack ifBody
    when (shouldTypeCheck && newStack /= stack) $
      report wp $ BlockIfNotStackNeutralError (stack, newStack)
    pure newStack
  FunctionCall targetName args -> do
    target <- retrieveFunction wp targetName
    when (length (funcArgs target) /= length args) $
      report wp $ FunctionCallWrongArgumentsNumberError target $ length args
    objs <- gets objects
    for_ (zip (funcArgs target) args) \(arg@(kind, _), expr) -> do
      let checkKind k = unless (k `canCastTo` kind) $
            report wp $ FunctionCallWrongArgumentTypeError target arg k
      case expr of
        ConstantName name -> case find (\(_, argName) -> argName == name) (funcArgs func) of
          Just (k, _) ->
            checkKind k
          Nothing -> case objs M.!? name of
            Just (WL _ (ValueObject vo)) ->
              checkKind (typeof vo)
            Just other ->
              report wp $ ExpectedValueGotFunctionError name other
            Nothing ->
              report wp $ ConstantNotFoundError name
        _ -> do
          result <- eval objs kind expr `onLeft` \err ->
            fatal wp err
          checkKind $ typeof result
    case funcPurity func of
      Impure -> pure stack
      Pure -> case stripPrefix (funcInput target) stack of
        Just ns -> pure $ funcOutput target ++ ns
        Nothing -> do
          report wp $ FunctionCallStackTypeError target $ reverse stack
          pure stack
  where
    shouldTypeCheck = funcPurity func == Pure

--------------------------------------------------------------------------------
-- helpers

guessStack
  :: Monad m
  => Function
  -> ([Type], [Type])
  -> WithLocation Instruction
  -> CompilerT m ([Type], [Type])
guessStack func (initStack, currentStack) wp@(WL _ inst) = case inst of
  FunctionCall n _ -> do
    target <- retrieveFunction wp n
    let missing = drop (length currentStack) $ funcInput target
        newInitStack = initStack ++ missing
        -- enforce that we check the stack for consistency
        alteredFunc = func { funcPurity = Pure }
    newCurrentStack <- processInstruction alteredFunc (currentStack ++ missing) wp
    pure (newInitStack, newCurrentStack)
  _ ->
    fatal wp (ConditionWrongInstructionError $ getEntry wp)

canCastTo :: Type -> Type -> Bool
BFChar `canCastTo` BFInt = True
a      `canCastTo` b     = a == b
