{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|

This module only exposes parseProgram, which takes a module name, the string
content, and attempts to parse it. It returns either a parse error or a Program,
which is simply a list of statements (see Grammar).

-}

module Parser (parseProgram) where

import Control.Arrow     (left)
import Control.Monad     (void)
import Data.These
import Text.Parsec
import Text.Parsec.Token qualified as P

import Diagnostics
import Grammar
import Location
import Misc

--------------------------------------------------------------------------------
-- exported parsing function

parseProgram :: String -> String -> Either Diagnostic Program
parseProgram = left toDiagnostic ... parse program
  where
    toDiagnostic pe = addPosition (errorPos pe) $ ParseError $ show pe

--------------------------------------------------------------------------------
-- grammar

program = whiteSpace *> statement `sepEndBy` many newline <* eof

-- statement

statement = do
  pos <- getPosition
  addPosition pos <$> choice
    [ include
    , constant
    , function
    ] <?> "statement (include directive, constant declaration, function declaration)"

include = do
  reserved "include"
  Include <$> sLiteral

constant = do
  reserved "const"
  (t, n) <- variable
  void $ symbol "="
  e <- expression
  return $ ConstantDecl $ Constant t n e

function = do
  reserved "def"
  (inlineKW, impureKW) <- fmap partitionHereThere $ many $ choice
    [ This Inline <$ reserved "inline"
    , That Impure <$ reserved "impure"
    ]
  rendering <- case inlineKW of
    []  -> pure Block
    [_] -> pure Inline
    _   -> fail "expecting at most one 'inline' keyword"
  purity <- case impureKW of
    []  -> pure Pure
    [_] -> pure Impure
    _   -> fail "expecting at most one 'impure' keyword"
  name <- identifier
  args <- parens $ commaSep variable
  (inputType, outputType) <- option ([], []) do
    i <- brackets $ commaSep typename
    void $ symbol "->"
    o <- brackets $ commaSep typename
    return (i, o)
  instructions <- braces $ many instruction
  return $ FunctionDecl $ Function
    { funcName = name
    , funcPurity = purity
    , funcRender = rendering
    , funcArgs = args
    , funcInput = reverse inputType
    , funcOutput = reverse outputType
    , funcBody = const instructions
    }

-- instructions

instruction = do
  pos <- getPosition
  addPosition pos <$> choice
    [ functionCall
    , ifb
    , loop
    , while
    , rawBrainfuck
    ] <?> "instruction (function call, if, loop, while, or raw brainfuck code)"

functionCall = do
  n <- identifier
  a <- option [] $ parens $ commaSep expression
  return $ FunctionCall n a

ifb = do
  reserved "if"
  c <- parens $ many instruction
  b <- braces $ many instruction
  return $ If c b

loop = do
  reserved "loop"
  b <- braces $ many instruction
  return $ Loop b

while = do
  reserved "while"
  c <- parens $ many instruction
  b <- braces $ many instruction
  return $ While c b

rawBrainfuck = do
  s <- many1 $ lexeme $ oneOf brainfuckChars
  return $ RawBrainfuck s

-- expressions

expression = choice
  [ ConstantName  <$> identifier
  , LiteralString <$> sLiteral
  , LiteralChar   <$> cLiteral
  , LiteralInt . fromInteger  <$> iLiteral
  ] <?> "expression (constant name or literal)"

-- variables and types

typename = choice [pstring, pint, pchar, pbool] <?> "type name"
  where
    pstring = BFString <$ lexeme (char 'S')
    pint    = BFInt    <$ lexeme (char 'I')
    pchar   = BFChar   <$ lexeme (char 'C')
    pbool   = BFBool   <$ lexeme (char 'B')

variable = do
  t <- typename
  n <- identifier
  return (t, n)

--------------------------------------------------------------------------------
-- parsec language definition

language :: P.TokenParser ()
language = P.makeTokenParser P.LanguageDef
  { P.commentStart    = "/*"
  , P.commentEnd      = "*/"
  , P.commentLine     = "//"
  , P.nestedComments  = True
  , P.identStart      = char '_' <|> letter
  , P.identLetter     = char '_' <|> alphaNum
  , P.opStart         = parserZero
  , P.opLetter        = parserZero
  , P.reservedOpNames = []
  , P.reservedNames   = ["if", "while", "loop", "include", "def", "const", "inline", "impure"]
  , P.caseSensitive   = True
  }

whiteSpace = P.whiteSpace    language
lexeme     = P.lexeme        language
symbol     = P.symbol        language
reserved   = P.reserved      language
identifier = P.identifier    language
parens     = P.parens        language
braces     = P.braces        language
brackets   = P.brackets      language
commaSep   = P.commaSep      language
cLiteral   = P.charLiteral   language
sLiteral   = P.stringLiteral language
iLiteral   = P.integer       language

--------------------------------------------------------------------------------
-- helpers

addPosition :: SourcePos -> a -> WithLocation a
addPosition p = WL $ SourceFile n l c
  where
    n = sourceName   p
    l = sourceLine   p
    c = sourceColumn p
