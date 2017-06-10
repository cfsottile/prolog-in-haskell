module Prolog
    ( parseAndAskIf
    , parseAndAskAll
    , askIf
    , askAll
    , genMap
    , isGround
    , Term(..)
    , Clause(..)
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Text.ParserCombinators.Parsec (ParseError)

import Datatypes
import Parser
import Printing

parseAndAskIf :: String -> String -> Either ParseError Bool
parseAndAskIf t p = compileAndAskIf <$> parseGoal t <*> parseProgram p

parseAndAskAll :: String -> String -> Either ParseError [Valuation]
parseAndAskAll t p = compileAndAskAll <$> parseGoal t <*> parseProgram p

compileAndAskIf :: Term -> Program -> Bool
compileAndAskIf t = askIf t . genMap

compileAndAskAll :: Term -> Program -> [Valuation]
compileAndAskAll t p = askAll t (genMap p)

askIf :: Term -> ProgramMap -> Bool
askIf t = not . null . askAll t

askAll :: Term -> ProgramMap -> [Valuation]
askAll t pm = ask t pm Map.empty

ask :: Term -> ProgramMap -> Valuation -> [Valuation]
ask p@(Pred name _) pm val = 
    let mayUnifyClauses = concat $ Map.lookup name pm
        valuationAndGoals' = valuationAndGoals (Rule p []) val
        explore' (val',gs) = explore gs pm val'
    in concat $ explore' <$> mapMaybe valuationAndGoals' mayUnifyClauses
-- ask _ = 

valuationAndGoals :: Clause -> Valuation -> Clause -> Maybe (Valuation,Goals)
valuationAndGoals c1 val c2 =
    ((\sub -> substitute sub <$> goals c2) <$>)
    <$> unifyClause c1 c2 val

substitute :: Substitution -> Term -> Term
substitute sub v@(Var _) = fromMaybe v (Map.lookup v sub)
substitute sub (Pred name args) = Pred name (substitute sub <$> args)

-- clauseHead :: Clause -> Term
-- clauseHead (Rule p@(Pred _ _) _) = p

goals :: Clause -> Goals
goals (Rule (Pred _ _) g) = g

-- | Produces the list of the valuations that satisfies all the goals (g:gs)
explore :: Goals -> ProgramMap -> Valuation -> [Valuation]
explore [] _ val = [val]
-- ask g pm val :: [Valuation]; explore gs pm :: Valuation -> [Valuation]
-- la mónada List mete no determinismo y genera todas las posibilidades
-- o sea, hace el backtracking gratis
explore (g:gs) pm val = ask g pm val >>= explore gs pm

-- | Takes a Program and produces a Map of Clause [Clause] where the key
-- just holds the name and no args nor goals, and the Clauses are those actual
-- program clauses that shares Name with key. This Map will be used to reduce
-- the amount of possible unifiers for a given goal.
genMap :: Program -> ProgramMap
genMap program = foldr genKeyValuePair Map.empty (nameClauses program)
    where
        nameUnifiers name = filter (nameUnify name) program
        genKeyValuePair name programMap =
            Map.insert name (nameUnifiers name) programMap

nameUnify :: Name -> Clause -> Bool
nameUnify name1 (Rule (Pred name2 _) _) = name1 == name2
nameUnify _ _ = False

-- | Reduces the program to a list of the Names of its Clauses.
nameClauses :: Program -> [Name]
nameClauses program = List.nub $ mapMaybe getName program

getName :: Clause -> Maybe Name
getName (Rule (Pred name _) _) = Just name
getName _ = Nothing

unifyClause :: Clause -> Clause -> Valuation -> Maybe Unifier
unifyClause (Rule p1 _) (Rule p2 _) val = unify p1 p2 (val, Map.empty)

-- | Takes two terms and a valuation. If terms unify, returns the compatible
-- valuation that allows the unification. If not, returns Nothing.
unify :: Term -> Term -> Unifier -> Maybe Unifier
unify v1@(Var _) v2@(Var _) (val,sub) = addSub v1 v2 (val,sub)
unify v@(Var _) t (val,sub) = addVal v t (val,sub)
unify t v@(Var _) (val,sub) = addVal v t (val,sub)
unify (Pred name1 args1) (Pred name2 args2) val
    | name1 == name2 && length args1 == length args2 = 
        argsUnify args1 args2 val
    | otherwise = Nothing
-- unify _ _ _ = Nothing

-- | Attempts to insert a variable substitution [v1/v2] in the Map. If v2 is
-- present as a key, checks wether its value and v1 are the same and performs
-- no changes. If not, the result is Nothing.
addSub :: Term -> Term -> Unifier -> Maybe Unifier
addSub v1@(Var name1) v2@(Var name2) (val,sub) = case Map.lookup v2 sub of
    Nothing -> Just (val, Map.insert v2 v1 sub)
    Just v' -> if v' == v1 then Just (val,sub) else Nothing

-- | Attempts to insert a variable instantiation v = term in the Map. If v is
-- present as a key, the result is Maybe Unifier from unifying
-- v's value and term.
addVal :: Term -> Term -> Unifier -> Maybe Unifier
addVal v@(Var _) term (val,sub) = case Map.lookup v val of
    Nothing -> Just (Map.insert v term val, sub)
    Just t -> unify t term (val,sub) -- mmmmm

argsUnify :: [Term] -> [Term] -> Unifier -> Maybe Unifier
argsUnify [] [] (val,sub) = Just (val,sub)
-- unify a1 a2 val :: Maybe Unifier
-- argsUnify as1 as2 :: Unifier -> Maybe Unifier
-- La mónada Maybe se encarga de continuar ante Just, y de cortar ante Nothing
argsUnify (a1:as1) (a2:as2) (val,sub) = unify a1 a2 (val,sub) >>= argsUnify as1 as2

isGround :: Term -> Bool
isGround (Var _) = False
isGround (Pred name args) = all isGround args