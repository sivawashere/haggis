module Haggis.Lexer where

import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T
import Text.Parsec ((<|>), letter, alphaNum, char, oneOf)

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style where style = emptyDef {
	T.commentLine     = "#",
	T.identStart      = letter,
	T.identLetter     = alphaNum <|> char '_',
	T.opStart         = T.opLetter style,
	T.opLetter        = oneOf "NOTANDRmod=!<>+-*/&^",
	T.reservedOpNames = ["NOT", "AND", "OR", "=", "!=", "<", "<=", ">",
	                     ">=", "+", "-", "*", "/", "mod", "&", "^"],
	T.reservedNames   = ["ARRAY", "AS", "BOOLEAN", "CHARACTER", "CLASS",
	                     "CLOSE", "CONSTRUCTOR", "CREATE","DECLARE",
	                     "DISPLAY", "DO", "EACH", "ELSE", "END", "FOR",
	                     "FOREACH", "FROM", "FUNCTION", "IF", "INHERITS",
	                     "INITIALLY", "INTEGER", "IS", "KEYBOARD", "NOT",
	                     "OF", "OPEN", "OVERRIDES", "PARALLEL", "PROCEDURE",
	                     "REAL", "RECEIVE", "RECORD", "REPEAT", "RETURN",
	                     "RETURNS", "SEND", "SEQUENCE", "SET", "STEP",
	                     "STRING", "THEN", "THIS", "TIMES", "TO", "TYPE",
	                     "UNTIL", "VAR", "WHEN", "WHILE", "WITH", "false", "true"] }

int      = T.decimal       lexer
real     = T.float         lexer
str      = T.stringLiteral lexer
res      = T.reserved      lexer
resOp    = T.reservedOp    lexer
ident    = T.identifier    lexer
brackets = T.brackets      lexer
commaSep = T.commaSep      lexer
parens   = T.parens        lexer
ws       = T.whiteSpace    lexer