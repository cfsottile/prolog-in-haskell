{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Parsec.Error (ParseError)
import System.IO
import Web.Scotty
import Data.Text.Lazy (pack)
import Data.List (intercalate)

import Datatypes (ProgramMap, Substitution)
import Prolog
import Parser
import Printing

main = scotty 8080 $ do
    get "/" $
        html . pack $ concat [bodyHtml, textareaHtml "Prolog program goes here", goalHtml "Goal goes here", closingBody]
    post "/ask" $ do
        program <- param "program"
        goal <- param "goal"
        let cont = continue goal program
        either (cont . error') (cont . answer) (mainAsk goal program)

continue goal program response =
    html . pack $ concat [bodyHtml, textareaHtml program, goalHtml goal, response, closingBody]

answer :: [Doc] -> String
answer a = answerHtml ++ "<ul><li>" ++ intercalate "</li><br><li>" (map show a) ++ "</li></ul>"

error' :: ParseError -> String
error' e = errorHtml ++ show e

mainAsk :: String -> String -> Either ParseError [Doc]
mainAsk goal program = do
    goal' <- parseGoal goal
    if isGround goal' then showAnswerIfHtml <$> parseAndAskIf goal program
                      else let ans = parseAndAskAll goal program
                           in  if null ans then showAnswerIfHtml <$> Right False
                                           else showAnswerAllHtml <$> ans
                                        --    else (showValuation <$>) <$> ans


bodyHtml = "<html><head><title>prolog-hs</title><style>body { font-family: \"Helvetica\", serif }</style></head><body>\
    \<h1>prolog-hs</h1>\

    \<h2>What is it?</h2>\
    
    \<p> prolog-hs is a Haskell module developed by Cristian Sottile for the Functional Programming course of the National University of La Plata. This webapp is Scotty based, and uses the prolog-hs module. </p>\
    
    \<p> As you may guess from the name, it is a Haskell based Prolog. </p>\
    
    \<h2>How to use it?</h2>\
    
    \<p> You need to provide a *Prolog program*, and a *goal*, and prolog-hs will answer: </p>\
    
    \<p> * Sí or No, if the goal didn't contain variables; </p>\
    \<p> * the substitutions that satisfies your query, in case there was any, if the goal did contain variables. </p>\
    \<div>Examples: <button onclick=\"ancestors()\">Ancestors</button> <button onclick=\"lists()\">Lists</button></div>"

textareaHtml str = concat ["<textarea id=\"prog\" form=\"askForm\" name=\"program\" cols=\"80\" rows=\"15\">", str, "</textarea>"]

goalHtml str = concat
    [ "<form action=\"ask\" id=\"askForm\" method=\"post\">"
    , "Goal:<br> <input type=\"text\" size=80 id=\"g\" name=\"goal\" value=\"", str, "\">"
    , "<input type=\"submit\" value=\"Submit\">"
    , "</form>"
    ]

answerHtml = "<p>Answer:</p>"

errorHtml = "<p>Error:</p>"

closingBody = "<script>function lists() { document.getElementById('prog').value = 'append(nil,YS,YS).\\nappend(cons(X,XS),YS,ZS) :- append(XS,YS,WS), equal(cons(X,WS),ZS).\\nequal(nil,nil).\\nequal(cons(X,XS),cons(X,YS)) :- equal(XS,YS).'; document.getElementById('g').value = 'append(cons(a,nil),cons(b,nil),ZS).' } function ancestors() { document.getElementById('prog').value = 'parent(father,child).\\nparent(mother,child).\\nparent(grandfather,mother).\\nparent(grandmother,mother).\\nparent(greatgrandfather,grandfather).\\nancestor(X,Y) :- parent(X,Y).\\nancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).'; document.getElementById('g').value = 'ancestor(X,Y).' } </script> </body> </html>"