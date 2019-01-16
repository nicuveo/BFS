module Diagnostics where

import           Text.Printf

import           Grammar
import           Module
import           Object


type Diagnostic  = WithLocation Error
type Diagnostics = [Diagnostic]

data Error = ConstantAlreadyDefinedError           String (WithLocation Object)
           | FunctionAlreadyDefinedError           String (WithLocation Object)
           | ArgumentNameShadowsObjectWarning      String (WithLocation Object)
           | IntLiteralError                       Type Int
           | CharLiteralError                      Type Char
           | StringLiteralError                    Type String
           | ImplicitCastError                     Type Type
           | FunctionTypeDeclarationError          Function [Type]
           | FunctionCallWrongArgumentsNumberError Function Int
           | FunctionCallWrongArgumentTypeError    Function Variable Type
           | FunctionCallStackTypeError            Function [Type]
           | BlockIfNotStackNeutralError           ([Type], [Type])
           | BlockLoopNotStackNeutralError         ([Type], [Type])
           | BlockWhileNotStackNeutralError        ([Type], [Type])
           | ConditionWrongTypeError               ([Type], [Type])
           | ConditionWrongInstructionError        Instruction
           | ExpectedValueGotFunctionError         String (WithLocation Object)
           | ExpectedFunctionGotValueError         String (WithLocation Object)
           | PureFunctionsContainsImpureCodeError  String
           | ConstantNotFoundError                 String
           | FunctionNotFoundError                 String
           | DuplicateArgumentNamesError           [String]
           | ParseError                            String
           | FileNotFoundError                     String

isError :: Diagnostic -> Bool
isError (WL _ (ArgumentNameShadowsObjectWarning _ _)) = False
isError _ = True

instance Show Error where
  show (ConstantAlreadyDefinedError      n wo)         = printf "name error: constant %s conflicts with:\n  %s"   n $ show wo
  show (FunctionAlreadyDefinedError      n wo)         = printf "name error: function %s conflicts with:\n  %s"   n $ show wo
  show (ArgumentNameShadowsObjectWarning n wo)         = printf "warning: argument %s shadows object:\n  %s" n $ show wo
  show (IntLiteralError    k x)                        = printf "type error: int constant %d cannot cast to %s"    x $ show k
  show (CharLiteralError   k x)                        = printf "type error: char constant %c cannot cast to %s"   x $ show k
  show (StringLiteralError k x)                        = printf "type error: string constant %s cannot cast to %s" x $ show k
  show (ImplicitCastError  d s)                        = printf "type error: cannot cast %v to %v" (show s) $ show d
  show (FunctionTypeDeclarationError f ks)             = printf "error: inferred function result stack for function %s (%s) is different from declared result stack (%s)" (funcName f) (show ks) $ show $ funcOutput f
  show (FunctionCallWrongArgumentsNumberError f n)     = printf "error: function %s expects %d arguments, but only %d given" (funcName f) (length $ funcArgs f) n
  show (FunctionCallWrongArgumentTypeError f (e, x) a) = printf "type error: in call to function %s, arg %s expects a value of type %s, but got a value of type %s" (funcName f) x (show e) $ show a
  show (FunctionCallStackTypeError f x)                = printf "type error: in call to function %s, expecting the stack to be %s, but was %s" (funcName f) (show $ funcInput f) $ show x
  show (BlockIfNotStackNeutralError    (i,o))          = printf "error: if block is unbalanced: %s -> %s" (show i) $ show o
  show (BlockLoopNotStackNeutralError  (i,o))          = printf "error: loop block is unbalanced: %s -> %s" (show i) $ show o
  show (BlockWhileNotStackNeutralError (i,o))          = printf "error: while block is unbalanced: %s -> %s" (show i) $ show o
  show (ConditionWrongTypeError (i,o))                 = printf "error: condition has wrong type: want (%s -> %s), but got (%s -> %s)" (show i) (show $ i ++ [BFBool]) (show i) (show o)
  show (ConditionWrongInstructionError _)              = printf "error: only function calls are allowd in conditions"
  show (ExpectedValueGotFunctionError n (WL l _))      = printf "error: expected %s to be a constant, but was a function (defined at %s)" n $ show l
  show (ExpectedFunctionGotValueError n (WL l _))      = printf "error: expected %s to be a function, but was a constant (defined at %s)" n $ show l
  show (PureFunctionsContainsImpureCodeError n)        = printf "error: function %s is declared as pure but contains impure code" n
  show (ConstantNotFoundError x)                       = printf "name error: constant %s not found" x
  show (FunctionNotFoundError x)                       = printf "name error: function %s not found" x
  show (DuplicateArgumentNamesError as)                = printf "error: duplicate argument names: %s" $ unwords as
  show (ParseError s)                                  = "error: " ++ s
  show (FileNotFoundError f)                           = "error: could not find a file named " ++ f
