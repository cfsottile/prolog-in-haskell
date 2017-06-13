module Parser
    ( parseProgram
    , parseGoal
    , parseClause
    ) where

import Text.ParserCombinators.Parsec

import Datatypes

parseProgram :: String -> Either ParseError Program
parseProgram = parse program "Prolog program syntax error"

parseGoal :: String -> Either ParseError Term
parseGoal = parse (term <* char '.') "Goal syntax error"

parseClause = parse (clause <* char '.') "Clause syntax error"

program :: Parser Program
program = many junk >> clause `endBy1` lineEnd

clause :: Parser Clause
clause = do
    head <- clauseHead
    body <- option [] clauseBody
    return $ Rule head body

clauseHead :: Parser Term
clauseHead = predicate

clauseBody :: Parser [Term]
clauseBody = spaces >> string ":-" >> spaces >> listOfTerms

term :: Parser Term
term = var <|> predicate

var :: Parser Term
var = do
    c  <- upper
    cs <- many alphaNum
    return $ Var (c:cs)

predicate :: Parser Term
predicate = do
    name <- predicateName
    args <- option [] predicateArgs
    return $ Pred name args

predicateName :: Parser String
predicateName = do
    c  <- lower
    cs <- many alphaNum
    return (c:cs)

predicateArgs :: Parser [Term]
predicateArgs = between (char '(') (char ')') listOfTerms

listOfTerms :: Parser [Term]
listOfTerms = term `sepBy1` (char ',' >> spaces)

junk :: Parser ()
junk = skipMany1 space <|> comment

comment :: Parser ()
comment = between (char '%') (char '\n') (skipMany (noneOf "\n"))

lineEnd :: Parser ()
lineEnd = char '.' >> skipMany junk