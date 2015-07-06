module Haggis.Parser(parse) where

import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.String
import Control.Applicative ((<*))

import Haggis.Lexer
import Haggis.AST

throwWS = (>>) (skipMany1 space)

keyLit word val = res word >> return val

call :: (Ident -> [Expr] -> a) -> Parser a
call f = do
	id <- ident
	args <- parens $ commaSep expr
	return $ f id args

loc = {-try deref <|>-} id where
	id    = fmap Id ident
	{-deref = do
		array <- choice [strLit, arrayLit, parenExpr, call FuncCall]
		index <- brackets expr
		return $ Deref array index-}

baseExpr = (choice
	[throwWS expr, try $ call FuncCall, parenExpr, arrayLit,
	 try floatLit, intLit, strLit, boolLit, keyLit "KEYBOARD" Keyboard,
	 keyLit "DISPLAY" Display, location])
	<?> "base expression" where
	intLit    = fmap IntLit $ fmap fromIntegral int
	floatLit  = fmap FloatLit real
	boolLit   = b "true" True <|> b "false" False where
	                b name val = res name >> (return $ BoolLit val)
	strLit    = fmap StrLit str
	arrayLit  = fmap ArrayLit $ brackets $ commaSep expr
	location  = fmap Location loc
	parenExpr = fmap ParenExpr $ parens expr

expr = buildExpressionParser
	[[unOp  "-"   Negate,
	  unOp  "NOT" Not],
	 [binOp "^"   (RealExpr Exp)    AssocRight],
	 [binOp "&"   Concat AssocRight],
	 [binOp "*"   (RealExpr Mul)    AssocLeft, binOp "/"  (RealExpr Div)  AssocLeft,
	  binOp "mod" Mod    AssocLeft],
	 [binOp "+"   (RealExpr Add)    AssocLeft, binOp "-"  (RealExpr Sub)  AssocLeft],
	 [binOp "="   (EqExpr   Equ)    AssocLeft, binOp "!=" (EqExpr   NEqu) AssocLeft,
	  binOp "<"   (RealExpr Lt  )   AssocLeft, binOp "<=" (RealExpr LtEq) AssocLeft,
	  binOp ">"   (RealExpr GtEq)   AssocLeft, binOp ">=" (RealExpr GtEq) AssocLeft],
	 [binOp "AND" (BoolExpr And)    AssocLeft, binOp "OR" (BoolExpr And)  AssocLeft]]
	baseExpr <?> "expression" where
		unOp  name op = Prefix (resOp name >> return op)
		binOp name op = Infix  (resOp name >> return op)

baseType = choice $ map try
	[keyLit "STRING"    STRING,
	 keyLit "INTEGER"   INTEGER,
	 keyLit "REAL"      REAL,
	 keyLit "BOOLEAN"   BOOLEAN,
	 keyLit "CHARACTER" CHARACTER]

block = fmap Block $ many cmd

assign = do
	res "SET"
	l <- loc
	res "TO"
	e <- expr
	return $ Assign l e

ifCmd = do
	res "IF"
	cond <- expr
	res "THEN"
	conseq <- block
	alt <- option Null $ res "ELSE" >> block
	res "END"
	res "IF"
	return $ If cond conseq alt

rep = do
	res "REPEAT"
	body <- block
	res "UNTIL"
	cond <- expr
	return $ Repeat body cond

repTimes = do
	res "REPEAT"
	times <- expr
	res "TIMES"
	body <- block
	res "END"
	res "REPEAT"
	return $ RepTimes times body

for = do
	res "FOR"
	index <- ident
	res "FROM"
	l <- expr
	res "TO"
	u <- expr
	res "DO"
	body <- block
	res "END"
	res "FOR"
	return $ For index l u body

forEach = do
	x <- try $ res "FOREACH" <|> (res "FOR" >> res "EACH")
	index <- ident
	res "FROM"
	src <- expr
	res "DO"
	body <- block
	res "END"
	try $ res "FOREACH" <|> (res "FOR" >> res "EACH")
	return $ ForEach index src body

input = do
	res "RECEIVE"
	l <- loc
	res "FROM"
	bT <- parens baseType
	e <- expr
	return $ Receive l bT e

output = do
	res "SEND"
	src <- expr
	res "TO"
	dest <- expr
	return $ Send src dest

cmd = choice
	[throwWS cmd, assign, ifCmd, try repTimes, rep, try forEach,
	 for, input, output, call ProcCall]

program = ws >> many cmd <* eof

parse :: FilePath -> IO Program
parse path = do
	res <- parseFromFile program path
	case res of
		Left  err -> error $ show err
		Right ast -> return ast