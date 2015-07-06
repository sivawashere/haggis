module Haggis.Types where

data Type = TInt
		  | TReal
		  | TStr
		  | TBool
		  | TArr Type
		  | TGuiIn
		  | TGuiOut
		  | TVoid deriving Eq
  
instance Show Type where
    show t = case t of
        TInt    -> "integer"
        TReal   -> "real number"
        TStr    -> "string"
        TBool   -> "boolean"
        TArr e  -> "array of " ++ show e ++ "s"
        TGuiIn  -> "input stream"
        TGuiOut -> "output stream"