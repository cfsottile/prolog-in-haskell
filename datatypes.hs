module Datatypes where

import qualified Data.Map as Map

type Name = String
type Args = [Term]
data Term = Var Name | Pred Name Args deriving (Eq,Show,Ord)
type Goals = [Term]
data Clause = Rule Term Goals deriving (Eq,Show,Ord) -- Rule's first argument must be Pred
type Program = [Clause]

type Valuation = Map.Map Term Term -- Key must be Var
type Substitution = Map.Map Term Term -- Key must be Var
type Unifier = (Valuation,Substitution)
type ProgramMap = Map.Map Name [Clause]