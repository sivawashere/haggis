module Haggis.Scoping.Env where

import Control.Monad.Reader

import Haggis.AST
import Haggis.Types
import qualified Haggis.Scoping.Scope as S

type Env = Reader S.Scope

lookup :: Ident -> Env (Maybe S.Value)
lookup = asks . S.lookup

--assign :: Ident -> Value -> 
assign k v = local $ S.insert k v