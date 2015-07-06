module Haggis.Scoping.Scope where

import Prelude hiding (lookup)
import qualified Data.Map as M

import Haggis.AST
import Haggis.Types

data Value = Value Type Expr deriving (Eq, Show)

data Scope = Scope {
	outer :: Maybe Scope,
	scope :: M.Map Ident Value
} deriving (Eq, Show)

lookup :: Ident -> Scope -> Maybe Value
lookup k (Scope o s)
	| k `M.member` s = k `M.lookup` s
	| otherwise      = o >>= lookup k

--member :: Ord a => a -> Scope a b -> Bool
--member k (Scope o s) = k `M.member` s || maybe False (member k) o

insert :: Ident -> Value -> Scope -> Scope
insert k v@(Value t e) (Scope o s)
	| k `M.member` s = let Value to _ = s M.! k in
		if to == t || to == TReal && t == TInt then
			Scope o $ M.insert k v s
		else error $ "Type mismatch: Tried to assign " ++ k ++ " of type " ++ show to ++ " to " ++ show e ++ " of type " ++ show t
	| o == Nothing   = Scope o $ M.insert k v s
	| otherwise      = Scope (fmap (insert k v) o) s

empty = Scope Nothing M.empty
{-
test = Scope {
	outer = Just $ Scope {
		outer = Just $ Scope {
			outer = Nothing,
			scope = M.fromList [('a', 1)]
		},
		scope = M.fromList [('b', 2), ('c', 3)]
	},
	scope = M.fromList [('d', 4)]
}-}