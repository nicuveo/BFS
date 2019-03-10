{- Parser.hs

This file only exposes parseProgram, which takes a module name, the
string content, and attempts to parse it. It returns either a parse
error or a Program, which is simply a list of statements (see
Grammar).

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parser (parseProgram) where



-- imports

import           Control.Arrow     (left)
import           Text.Parsec
import qualified Text.Parsec.Token as P

import           Diagnostics
import           Grammar
import           Misc
import           Module



-- program

parseProgram :: String -> String -> Either Diagnostic Program
parseProgram = left toDiagnostic ... parse program
  where toDiagnostic pe = addPosition (errorPos pe) $ ParseError $ show pe

program = whiteSpace *> statement `sepEndBy` many newline <* eof



-- language definition

language = P.makeTokenParser P.LanguageDef { P.commentStart    = "/*"
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
  symbol "="
  e <- expression
  return $ ConstantDecl $ Constant t n e

function = do
  reserved "def"
  k <- many $ choice keywords
  let p = impure `notElem` k
      l = inline `elem` k
  n <- identifier
  a <- parens $ commaSep variable
  (i, o) <- option ([], []) $ do
    i <- brackets $ commaSep typename
    symbol "->"
    o <- brackets $ commaSep typename
    return (i, o)
  b <- braces $ many instruction
  return $ FunctionDecl $ Function n p l a i o $ const b



-- instructions

instruction = do
  pos  <- getPosition
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
  where pstring = BFString <$ lexeme (char 'S')
        pint    = BFInt    <$ lexeme (char 'I')
        pchar   = BFChar   <$ lexeme (char 'C')
        pbool   = BFBool   <$ lexeme (char 'B')

variable = do
  t <- typename
  n <- identifier
  return (t, n)



-- helpers

addPosition :: SourcePos -> a -> WithLocation a
addPosition p = WL $ SourceFile n l c
  where n = sourceName   p
        l = sourceLine   p
        c = sourceColumn p

inline = 0
impure = 1
keywords = [ inline <$ reserved "inline"
           , impure <$ reserved "impure"
           ]

brainfuckChars = "+-,.<>[]#"
