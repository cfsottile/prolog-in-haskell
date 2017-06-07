module Datatypes where

type Name = String
type Args = [Term]
data Term = Var Name | Pred Name Args deriving (Eq,Ord)
type Goals = [Term]
data Clause = Rule Term Goals deriving (Eq,Show,Ord) -- Rule's first argument must be Pred
type Program = [Clause]

instance Show Term where
    show (Pred name args) | null args = name
                          | otherwise = name ++ "(" ++ showArgs args ++ ")"
    show (Var v)          = v

showArgs :: Args -> String
showArgs []     = ""
showArgs [a]    = show a
showArgs (a:as) = show a ++ ',' : showArgs as