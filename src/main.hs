module Main where

import Datatypes (ProgramMap, Substitution)
import Prolog
import Parser
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Printing
import System.IO
import Web.Scotty

main = scotty 3000 $ do
    get "/" $ do
        body1 <- readFile "body1.html"
        html $ concat [body1, textarea "Prolog program goes here", goal "Goal goes here"]
    get "/:program/:goal" $ do
        program <- param "program"
        goal <- param "goal"
        let cont = continue goal program
        either (cont . error) (cont . answer) (mainAsk goal program)

continue goal program response = do
    body1 <- readFile "body1.html"
    html $ concat [body1, textarea (show program), goal (show goal)]

answer a =
    let answerHtml = readFile "answer.html"
    in  answerHtml ++ a

error e = 
    let errorHtml = readFile "error.html"
    in  errorHtml ++ a
        
mainAsk goal program = do
    goal' <- parseGoal goal
    if isGround goal' then parseAndAskIf goal program
                      else parseAndAskAll goal program

textarea str = concat ["<textarea form=\"askForm\" cols=\"80\" rows=\"15\">", str, "</textarea>"]

goal str = concat ["<form action=\"ask\" id=\"askForm\">Goal: <input type=\"text\" value", str, "></form>"]