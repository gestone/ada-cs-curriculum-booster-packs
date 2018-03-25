{-# LANGUAGE FlexibleContexts #-}

module Parser (parseBitLangExpr) where

import           BitLanguage
import           Control.Monad (void)
import           Data.Int (Int32)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

integer :: Parser Integer
integer = lexeme L.integer

parens :: Parser BitExpr -> Parser BitExpr
parens = between (symbol "(") (symbol ")")

term = parens expr
    <|> BitInt . fromInteger <$> integer
    <?> "term"

table = [
          [unary "~" BitNot],
          [binary "<<" BitLeftShift, binary ">>" BitRightShift]
        , [binary "&" BitAnd]
        , [binary "^" BitXOr]
        , [binary "|" BitOr]
        ]

binary name f = InfixL (symbol name >> return f)
unary  name f = Prefix  (f <$ symbol name)

expr :: Parser BitExpr
expr = makeExprParser term table <?> "expression"

parseBitLangExpr = parse expr ""
