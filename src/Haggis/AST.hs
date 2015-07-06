module Haggis.AST where

type Ident = String

data Loc = Id Ident | Deref Expr Expr deriving Eq

instance Show Loc where
    show loc = case loc of
      Id id     -> id
      Deref a i -> show a ++ "[" ++ show i ++ "]"

data BaseType = STRING | INTEGER | REAL | BOOLEAN | CHARACTER deriving Show

data BoolOp = And | Or deriving (Eq, Show)
data EqOp   = Equ | NEqu deriving (Eq, Show)
data RealOp = Add | Sub | Mul | Div | Exp | Lt | LtEq | Gt | GtEq deriving (Eq, Show)

data Expr = IntLit    Int
          | FloatLit  Double
          | StrLit    String
          | BoolLit   Bool
          | ArrayLit  [Expr]
          | Location  Loc
          | ParenExpr Expr
          | FuncCall  Ident [Expr]
          | Negate    Expr
          | Not       Expr
          | BoolExpr  BoolOp Expr Expr
          | EqExpr    EqOp  Expr  Expr
          | RealExpr  RealOp Expr Expr
          | Mod       Expr  Expr
          | Concat    Expr  Expr
          | Keyboard
          | Display deriving (Eq, Show)
{-
instance Show Expr where
    show e = case e of
        IntLit i -> show i
        FloatLit l -> show l
        StrLit s -> s
        BoolLit b -> show b
        ArrayLit a -> show a
        Location loc -> show loc
        ParenExpr p -> "(" ++ show p ++ ")"
        FuncCall f args -> show f ++ "(" ++  ++ ")"
-}
data Command = Null
             | Block [Command]
             | Assign   Loc     Expr
             | If       Expr    Command  Command
             | While    Expr    Command
             | Repeat   Command Expr
             | RepTimes Expr    Command
             | For      Ident   Expr     Expr Command
             | ForEach  Ident   Expr     Command
             | Receive  Loc     BaseType Expr
             | Send     Expr    Expr
             | ProcCall Ident   [Expr] deriving Show

data Construct = E Expr | C Command

instance Show Construct where
    show c = case c of
        E e -> show e
        C c -> show c

type Program = [Command]