{-# LANGUAGE OverloadedStrings #-}
module Mallet.Core.Parse
    ( Parser
    , parseTerm
    )
where

import           Control.Monad                  ( guard )
import           Data.Char                      ( isLetter )
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import           Data.Functor                   ( void )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Numeric.Natural                ( Natural )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lexer

import           Mallet.Core.Term

type Parser = Parsec Void Text

parseBinding :: Parser CoreTerm
parseBinding = do
    lambda
    lparen
    var <- identifier
    colon
    domain <- parseTerm
    rparen
    comma
    body <- parseTerm
    let binding = makeBinding var domain body
    pure binding

parseFactor :: Parser CoreTerm
parseFactor = parseBinding <|> parseType <|> parseVar <|> parens parseTerm

parseTerm :: Parser CoreTerm
parseTerm = foldr1 App <$> some parseFactor

parseType :: Parser CoreTerm
parseType = do
    keyword "Type"
    universe <- option 0 natural
    pure (Type universe)

parseVar :: Parser CoreTerm
parseVar = makeVar <$> identifier

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 empty empty

keyword :: Text -> Parser ()
keyword k = void $ lexeme (chunk k <* notFollowedBy letterChar)

keywords :: HashSet Text
keywords = HashSet.fromList ["Type"]

colon :: Parser ()
colon = symbol ":"

comma :: Parser ()
comma = symbol ","

lambda :: Parser ()
lambda = symbol "\\"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

lparen :: Parser ()
lparen = symbol "("

rparen :: Parser ()
rparen = symbol ")"

parens :: Parser a -> Parser a
parens = between lparen rparen

symbol :: Text -> Parser ()
symbol = void . Lexer.symbol spaceConsumer

natural :: Parser Natural
natural = lexeme Lexer.decimal

identifier :: Parser Text
identifier = do
    ident <- lexeme (takeWhile1P (Just "letter") isLetter)
    guard (not $ HashSet.member ident keywords)
    pure ident
