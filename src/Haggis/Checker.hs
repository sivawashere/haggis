{-# LANGUAGE LambdaCase #-}

module Haggis.Checker(check) where

import Prelude hiding (lookup)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (runReader)

import Haggis.AST
import Haggis.Types
import Haggis.Scoping.Env
import Haggis.Scoping.Scope hiding (lookup)

data TypeDescr = T Type
			   | TEither TypeDescr TypeDescr
			   | TAnd    TypeDescr TypeDescr
			   | TAny
			   | TAnyArr

instance Show TypeDescr where
	show td = case td of
		T t           -> show t
		TEither t1 t2 -> "either " ++ show t1 ++ " or " ++ show t2
		TAnd    t1 t2 -> show t1 ++ " and " ++ show t2
		TAny          -> "any type"
		TAnyArr       -> "array of any type"

data TypeErr = Mismatch TypeDescr TypeDescr Construct
             | NotInScope Ident

instance Show TypeErr where
	show te = case te of
		Mismatch exp got cons ->
			"Type mismatch: Expected "
			++ show exp
			++ " but got "
			++ show got ++
			" instead in "
			++ show cons
		NotInScope id ->
			"Identifier "
			++ id
			++ " not in scope"

type Check = ExceptT TypeErr Env

{-(===) :: Type -> Type -> Bool
TInt  === TReal = True
TReal === TInt  = True
a     === b    = a == b-}

typeof :: Construct -> Check Type
typeof c@(E expr) = case expr of
	IntLit i -> return TInt
	FloatLit r -> return TReal
	StrLit s -> return TStr
	BoolLit b -> return TBool
	--ArrayLit a -> mapM_ typeof $ map E a
	Location l -> case l of
		Id id -> do
			var <- lift $ lookup id
			case var of
				Just (Value t _) -> return t
				Nothing -> throwE $ NotInScope id
		Deref arr ind -> do
			tarr <- typeof $ E arr
			tind <- typeof $ E ind
			case (tarr, tind) of
				(TArr a, TInt) -> return a
				_              -> throwE $ Mismatch (TAnd TAnyArr (T TInt)) (TAnd (T tarr) (T tind)) c
	ParenExpr p -> typeof $ E p
	--FuncCall f args -> typeof $ Location $ 
	Negate e -> (typeof $ E e) >>= \case
		TInt  -> return TInt
		TReal -> return TReal
		t     -> throwE $ Mismatch (T TReal) (T t) c
	Not e -> (typeof $ E e) >>= \case
		TBool -> return TBool
		t     -> throwE $ Mismatch (T TBool) (T t) c
	BoolExpr _ l r -> do
		tl <- typeof $ E l
		tr <- typeof $ E r
		case (tl, tr) of
			(TBool, TBool) -> return TBool
			_              -> throwE $ Mismatch (TAnd (T TBool) (T TBool)) (TAnd (T tl) (T tr)) c
	EqExpr _ l r -> do
		tl <- typeof $ E l
		tr <- typeof $ E r
		if tl /= tr then
			throwE $ Mismatch (TEither (TAnd (T tl) (T tl)) (TAnd (T tr) (T tr))) (TAnd (T tl) (T tr)) c
		else
			return tl
	RealExpr _ l r -> do
		tl <- typeof $ E l
		tr <- typeof $ E r
		case (tl, tr) of
			(TInt, TInt)   -> return TInt
			(TReal, TReal) -> return TReal
			(TInt, TReal)  -> return TReal
			(TReal, TInt)  -> return TReal
			_              -> throwE $ Mismatch (TAnd (T TReal) (T TReal)) (TAnd (T tl) (T tr)) c
	Mod l r -> do
		tl <- typeof $ E l
		tr <- typeof $ E r
		case (tl, tr) of
			(TInt, TInt) -> return TInt
			_            -> throwE $ Mismatch (TAnd (T TInt) (T TInt)) (TAnd (T tl) (T tr)) c
	Concat l r -> do
		tl <- typeof $ E l
		tr <- typeof $ E r
		case (tl, tr) of
			(TArr a, TArr b) | a == b -> return tl
			_                         -> throwE $ Mismatch (TAnd TAnyArr TAnyArr) (TAnd (T tl) (T tr)) c
	Keyboard -> return TGuiIn
	Display  -> return TGuiOut

-- idea: give scope a (type, value) instead of just value
-- during typechecking, have value be Prelude.undefined so you can only look at the type
-- but during execution, it'll be whatever the user sets it to
typeof c@(C cmd) = case cmd of
	Null -> return TVoid
	Block cs -> do
		mapM_ typeof $ map C cs
		return TVoid
	{-Assign loc@(Location l) e -> do
		case l of
			Id id -> lift $ assign id $ Value (typeof e) undefined
			Deref _ _ -> do
				TArr tarr <- typeof $ E loc
				lift $ assign ("$" ++ )
		typeof e
		return TVoid-}
	If cond conseq alt -> do
		tc <- typeof $ E cond
		if tc /= TBool then
			throwE $ Mismatch (T TBool) (T tc) c
		else do
			typeof $ C conseq
			typeof $ C alt
	While cond body -> do
		tc <- typeof $ E cond
		if tc /= TBool then
			throwE $ Mismatch (T TBool) (T tc) c
		else typeof $ C body
	Repeat body cond -> do
		tc <- typeof $ E cond
		if tc /= TBool then
			throwE $ Mismatch (T TBool) (T tc) c
		else typeof $ C body
	RepTimes times body -> do
		tt <- typeof $ E times
		if tt /= TInt then
			throwE $ Mismatch (T TInt) (T tt) c
		else typeof $ C body
	For id from to body -> do
		tf <- typeof $ E from
		tt <- typeof $ E to
		case (tf, tt) of
			(TInt, TInt) -> typeof $ C body
			_            -> throwE $ Mismatch (TAnd (T TInt) (T TInt)) (TAnd (T tf) (T tt)) c
	ForEach id e body -> do
		te <- typeof $ E e
		case te of
			TStr   -> typeof $ C body
			TArr _ -> typeof $ C body
			_      -> throwE $ Mismatch TAnyArr (T te) c
	{-Receive l _ e ->
		te <- typeof $ E e
		case te of
			TGuiIn -> typeof $ C $ Assign l-} 
	Send e1 e2 -> do
		typeof $ E e1
		t2 <- typeof $ E e2
		if t2 /= TGuiOut then
			throwE $ Mismatch (T TGuiOut) (T t2) c
		else return TVoid
	--ProcCall

check :: Program -> Program
check p = case runReader (runExceptT $ typeof $ C $ Block p) empty of
	Left err -> error $ show err
	Right _  -> p