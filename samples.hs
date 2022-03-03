module Test where

import Prolog
import Printing
import Parser

-- cnst :: String -> Term
-- cnst n = Pred n []

-- parsedProgram1 = 
--     [ Rule (Pred "parent" [cnst "silvia" , cnst "cristian"]) []
--     , Rule (Pred "parent" [cnst "domingo", cnst "cristian"]) []
--     , Rule (Pred "parent" [cnst "juan"   , cnst "silvia"  ]) []
--     , Rule (Pred "parent" [cnst "yolanda", cnst "silvia"  ]) []
--     , Rule (Pred "parent" [cnst "yolanda", cnst "carlos"  ]) []
--     , Rule (Pred "parent" [cnst "juan"   , cnst "carlos"  ]) []
--     , Rule (Pred "woman"  [cnst "silvia"])                   []
--     , Rule (Pred "woman"  [cnst "yolanda"])                  []
--     , Rule (Pred "man"    [cnst "domingo"])                  []
--     , Rule (Pred "man"    [cnst "juan"])                     []
--     , Rule (Pred "human"  [cnst "juan"])                     []
--     , Rule (Pred "human"  [cnst "yolanda"])                  []
--     , Rule (Pred "human"  [cnst "domingo"])                  []
--     , Rule (Pred "mother" [Var "M", Var "X"])                [ Pred "woman"  [Var "M"]
--                                                              , Pred "parent" [Var "M", Var "X"]
--                                                              ]
--     , Rule (Pred "father" [Var "P", Var "X"])                [ Pred "man"    [Var "P"]
--                                                              , Pred "parent" [Var "P", Var "X"]
--                                                              ]
--     , Rule (Pred "human"  [Var "X"])                         [ Pred "human"  [Var "P"]
--                                                              , Pred "parent" [Var "P", Var "X"]
--                                                              ]
--     , Rule (Pred "human"  [Var "X"])                         [ Pred "human"  [Var "P"]
--                                                              , Pred "parent" [Var "P", Var "X"]
--                                                              , Pred "human"  [Var "M"]
--                                                              , Pred "parent" [Var "M", Var "X"]
--                                                              ]
--     , Rule (Pred "siblings" [Var "X", Var "Y"])              [ Pred "parent" [Var "Z", Var "X"]
--                                                              , Pred "parent" [Var "Z", Var "Y"]
--                                                              ]
--     ]

-- gp11 = Pred "mother"   [Pred "yolanda"  [], Pred "silvia"   []]
-- gp12 = Pred "mother"   [Pred "silvia"   [], Pred "cristian" []]
-- gp13 = Pred "siblings" [Pred "silvia"   [], Pred "carlos"   []]
-- gp14 = Pred "mother"   [Var "A", Var "B"]

-- parsedProgram2 = 
--     [ Rule (Pred "human"  [cnst "cristian"])  []
--     , Rule (Pred "human"  [cnst "totoy"])     []
--     , Rule (Pred "human"  [cnst "maxi"])      []
--     , Rule (Pred "human"  [cnst "ruben"])     []
--     , Rule (Pred "humans" [Var "X", Var "Y"]) [ Pred "human" [Var "X"]
--                                               , Pred "human" [Var "Y"]
--                                               ]
--     ]

-- p2 = [ Rule (Pred "human" [cnst "cristian"]) [], Rule (Pred "human" [cnst "totoy"])    [], Rule (Pred "human" [cnst "maxi"])     [], Rule (Pred "human" [cnst "ruben"])    [], Rule (Pred "humans" [Var "X", Var "Y"]) [ Pred "human" [Var "X"] , Pred "human" [Var "Y"]]]

-- gp21 = Pred "human"  [cnst "cristian"]
-- gp22 = Pred "human"  [cnst "totoy"]
-- gp23 = Pred "human"  [cnst "maxi"]
-- gp24 = Pred "human"  [cnst "ruben"]
-- gp25 = Pred "humans" [Var "A", Var "B"]

unparsedProgram2 = "% this is a comment!\n\
                    \human(cristian).\n\
                    \human(totoy).\n\
                    \human(maxi).\n\
                    \human(ruben).\n\
                    \humans(X,Y) :- human(X), human(Y).\n\
                    \humans4(X,Y,W,Z) :- humans(X,Y), humans(W,Z).\n\
                    \humans8(A,B,C,D,E,F,G,H) :- humans4(A,B,C,D), humans4(E,F,G,H).\n\
                    \humans16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- humans8(A,B,C,D,E,F,G,H), humans8(I,J,K,L,M,N,O,P)."

gu21 = "human(cristian)."
gu22 = "human(totoy)."
gu23 = "human(maxi)."
gu24 = "human(ruben)."
gu25 = "humans(X,Y)."
gu26 = "humans16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)."

lists = "\
        \append(nil,Ys1,Ys1).\
        \append(cons(X2,Xs2),Ys2,Zs2) :- \
            \append(Xs2,Ys2,Ws2),\
            \equal(cons(X2,Ws2),Zs2).\

        \equal(nil,nil).\

        \equal(cons(X3,Xs3),cons(X3,Ys3)) :-\
            \equal(Xs3,Ys3)."

listas = parseProgram "append(nil,YS,YS).\
         \append(cons(X,XS),YS,ZS) :- append(XS,YS,WS), equal(cons(X,WS),ZS).\
         \equal(nil,nil).\
         \equal(cons(X,XS),cons(X,YS)) :- equal(XS,YS)."

clause = parseClause "append(cons(X,XS),YS,ZS) :- append(XS,YS,WS), equal(cons(X,WS),ZS)."

gl0 = parseGoal "append(cons(a,nil),cons(b,nil),ZS)."
gl1 = parseGoal "append(cons(a,cons(b,nil)), cons(c,cons(d,nil)), Zs)."
gl2 = parseGoal "append(cons(a,cons(b,nil)), Ys, cons(a,cons(b,cons(c,nil))))."
glistas1 = parseGoal "append(nil,YS,YS)."
glistas11 = parseGoal "append(XS,YS,WS)."
glistas12 = parseGoal "equal(cons(X,WS),ZS)."
glistas2 = parseGoal "equal(nil,nil)."
glistas3 = parseGoal "equal(XS,YS)."

nada = parseProgram "p(Z,X,X)."
gnada = parseGoal "p(n(A),n(B),n(A))."

nada2 = parseProgram "p(Z,X,X,Y)."
gnada2 = parseGoal "p(n(A),n(B),n(A),A)."

badString = ";"