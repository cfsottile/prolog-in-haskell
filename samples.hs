module Test where

import Prolog
import Printing

cnst :: String -> Term
cnst n = Pred n []

parsedProgram1 = 
    [ Rule (Pred "parent" [cnst "silvia" , cnst "cristian"]) []
    , Rule (Pred "parent" [cnst "domingo", cnst "cristian"]) []
    , Rule (Pred "parent" [cnst "juan"   , cnst "silvia"  ]) []
    , Rule (Pred "parent" [cnst "yolanda", cnst "silvia"  ]) []
    , Rule (Pred "parent" [cnst "yolanda", cnst "carlos"  ]) []
    , Rule (Pred "parent" [cnst "juan"   , cnst "carlos"  ]) []
    , Rule (Pred "woman"  [cnst "silvia"])                   []
    , Rule (Pred "woman"  [cnst "yolanda"])                  []
    , Rule (Pred "man"    [cnst "domingo"])                  []
    , Rule (Pred "man"    [cnst "juan"])                     []
    , Rule (Pred "human"  [cnst "juan"])                     []
    , Rule (Pred "human"  [cnst "yolanda"])                  []
    , Rule (Pred "human"  [cnst "domingo"])                  []
    , Rule (Pred "mother" [Var "M", Var "X"])                [ Pred "woman"  [Var "M"]
                                                             , Pred "parent" [Var "M", Var "X"]
                                                             ]
    , Rule (Pred "father" [Var "P", Var "X"])                [ Pred "man"    [Var "P"]
                                                             , Pred "parent" [Var "P", Var "X"]
                                                             ]
    , Rule (Pred "human"  [Var "X"])                         [ Pred "human"  [Var "P"]
                                                             , Pred "parent" [Var "P", Var "X"]
                                                             ]
    , Rule (Pred "human"  [Var "X"])                         [ Pred "human"  [Var "P"]
                                                             , Pred "parent" [Var "P", Var "X"]
                                                             , Pred "human"  [Var "M"]
                                                             , Pred "parent" [Var "M", Var "X"]
                                                             ]
    , Rule (Pred "siblings" [Var "X", Var "Y"])              [ Pred "parent" [Var "Z", Var "X"]
                                                             , Pred "parent" [Var "Z", Var "Y"]
                                                             ]
    ]

gp11 = Pred "mother"   [Pred "yolanda"  [], Pred "silvia"   []]
gp12 = Pred "mother"   [Pred "silvia"   [], Pred "cristian" []]
gp13 = Pred "siblings" [Pred "silvia"   [], Pred "carlos"   []]
gp14 = Pred "mother"   [Var "A", Var "B"]

parsedProgram2 = 
    [ Rule (Pred "negro"  [cnst "cristian"])  []
    , Rule (Pred "negro"  [cnst "totoy"])     []
    , Rule (Pred "negro"  [cnst "maxi"])      []
    , Rule (Pred "negro"  [cnst "ruben"])     []
    , Rule (Pred "negros" [Var "X", Var "Y"]) [ Pred "negro" [Var "X"]
                                              , Pred "negro" [Var "Y"]
                                              ]
    ]

-- p2 = [ Rule (Pred "negro" [cnst "cristian"]) [], Rule (Pred "negro" [cnst "totoy"])    [], Rule (Pred "negro" [cnst "maxi"])     [], Rule (Pred "negro" [cnst "ruben"])    [], Rule (Pred "negros" [Var "X", Var "Y"]) [ Pred "negro" [Var "X"] , Pred "negro" [Var "Y"]]]

gp21 = Pred "negro"  [cnst "cristian"]
gp22 = Pred "negro"  [cnst "totoy"]
gp23 = Pred "negro"  [cnst "maxi"]
gp24 = Pred "negro"  [cnst "ruben"]
gp25 = Pred "negros" [Var "A", Var "B"]

unparsedProgram2 = "% this is a comment!\n\
                    \negro(cristian).\n\
                    \negro(totoy).\n\
                    \negro(maxi).\n\
                    \negro(ruben).\n\
                    \negros(X,Y) :- negro(X), negro(Y).\n\
                    \negros4(X,Y,W,Z) :- negros(X,Y), negros(W,Z).\n\
                    \negros8(A,B,C,D,E,F,G,H) :- negros4(A,B,C,D), negros4(E,F,G,H).\n\
                    \negros16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- negros8(A,B,C,D,E,F,G,H), negros8(I,J,K,L,M,N,O,P)."

gu21 = "negro(cristian)."
gu22 = "negro(totoy)."
gu23 = "negro(maxi)."
gu24 = "negro(ruben)."
gu25 = "negros(X,Y)."
gu26 = "negros16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)."