module Prolog
    ( askIf
    , askAll
    , Term(..)
    , Clause(..)
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Control.Arrow (first)

type Name = String
type Args = [Term]
data Term = Var Name | Pred Name Args deriving (Eq,Show,Ord)
type Goals = [Term]
data Clause = Rule Term Goals deriving (Eq,Show,Ord) -- Rule's first argument must be Pred
type Program = [Clause]

type Valuation = Map.Map Term Term -- Key must be Var
type ProgramMap = Map.Map Name [Clause]

askIf :: Term -> Program -> Bool
askIf t p = not $ null $ askAll t p

askAll :: Term -> Program -> [Valuation]
askAll t p = ask t (genMap p) Map.empty

-- | Takes a goal (PredTerm), a program (map) and a current valuation and
-- collects all possible valuations that make it true. I.e., starts with a goal
-- says all valuations that satisfy it.
ask :: Term -> ProgramMap -> Valuation -> [Valuation]
ask p@(Pred name _) pm val = 
    let possibleUnifiers = concat $ Map.lookup name pm
        unifyingValAndGoals clause = (unifyClause (Rule p []) clause val, goals clause)
        unifiers = (first fromJust <$>)       -- [(Valuation,Goals)]
               <$> filter (isJust . fst)      -- [(Maybe Valuation,Goals)]
                $  unifyingValAndGoals        -- [(Maybe Valuation,Goals)]
               <$> possibleUnifiers           -- [Clause] -- Maybe [Clause]
        explore' (val',gs) = explore gs pm val'
    in concat $ explore' <$> unifiers

goals :: Clause -> Goals
goals (Rule (Pred _ _) g) = g

-- | Produces the list of the valuations that satisfies all the goals (g:gs)
explore :: Goals -> ProgramMap -> Valuation -> [Valuation]
explore [] _ val = [val]
-- ask g pm val :: [Valuation]; explore gs pm :: Valuation -> [Valuation]
-- la mónada List mete no determinismo y genera todas las posibilidades
-- o sea, me hace el backtracking gratis
explore (g:gs) pm val = ask g pm val >>= explore gs pm

-- | Takes a Program and produces a Map of Clause [Clause] where the key
-- just holds the name and no args nor goals, and the Clauses are those actual
-- program clauses that shares Name with key. This Map will be used to reduce
-- the amount of possible unifiers for a given goal.
genMap :: Program -> ProgramMap
genMap program = foldr genKeyValuePair Map.empty (nameClauses program)
    where
        nameUnifiers name = filter (nameUnify name) program
        genKeyValuePair name programMap = Map.insert name (nameUnifiers name) programMap

nameUnify :: Name -> Clause -> Bool
nameUnify name1 (Rule (Pred name2 _) _) = name1 == name2
nameUnify _ _ = False

-- | Reduces the program to a list of the Names of its Clauses.
nameClauses :: Program -> [Name]
nameClauses program = List.nub $ mapMaybe getName program

getName :: Clause -> Maybe Name
getName (Rule (Pred name _) _) = Just name
getName _ = Nothing

unifyClause :: Clause -> Clause -> Valuation -> Maybe Valuation
unifyClause (Rule p1 _) (Rule p2 _) val = unify p1 p2 val

-- | Takes two terms and a valuation. If terms unify, returns the compatible
-- valuation that allows the unification. If not, returns Nothing.
unify :: Term -> Term -> Valuation -> Maybe Valuation
unify v@(Var _) t val = addVal v t val
unify t v@(Var _) val = addVal v t val
unify (Pred name1 args1) (Pred name2 args2) val
    | name1 == name2 && length args1 == length args2 = argsUnify args1 args2 val
    | otherwise = Nothing
-- unify _ _ _ = Nothing

-- | Attempts to insert a variable instantiation in the Map. If the key was
-- present, checks if the associated value unify with the term ........ ???
addVal :: Term -> Term -> Valuation -> Maybe Valuation
addVal v@(Var _) term val = case Map.lookup v val of
    Nothing -> Just $ Map.insert v term val
    Just t -> unify t term val

argsUnify :: [Term] -> [Term] -> Valuation -> Maybe Valuation
argsUnify [] [] val = Just val
-- unify a1 a2 val :: Maybe Valuation
-- argsUnify as1 as2 :: Valuation -> Maybe Valuation
-- La mónada Maybe se encarga de continuar ante Just, y de cortar ante Nothing
argsUnify (a1:as1) (a2:as2) val = unify a1 a2 val >>= argsUnify as1 as2
    -- código anterior
    -- let u = unify a1 a2 val
    -- in case u of
    --     Just val' -> argsUnify as1 as2 val'
    --     Nothing -> Nothing
