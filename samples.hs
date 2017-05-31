module Test where

import Prolog

cnst :: String -> Term
cnst n = Pred n []

p1 = 
    [ Rule (Pred "parent" [cnst "silvia" , cnst "cristian"]) []
    , Rule (Pred "parent" [cnst "domingo", cnst "cristian"]) []
    , Rule (Pred "parent" [cnst "juan"   , cnst "silvia"  ]) []
    , Rule (Pred "parent" [cnst "yolanda", cnst "silvia"  ]) []
    , Rule (Pred "parent" [cnst "yolanda", cnst "carlos"  ]) []
    , Rule (Pred "parent" [cnst "juan"   , cnst "carlos"  ]) []
    , Rule (Pred "woman"  [cnst "silvia"])                  []
    , Rule (Pred "woman"  [cnst "yolanda"])                 []
    , Rule (Pred "man"    [cnst "domingo"])                 []
    , Rule (Pred "man"    [cnst "juan"])                    []
    , Rule (Pred "human"  [cnst "juan"])                    []
    , Rule (Pred "human"  [cnst "yolanda"])                 []
    , Rule (Pred "human"  [cnst "domingo"])                 []
    , Rule (Pred "mother" [Var "M", Var "X"])              [ Pred "woman"  [Var "M"]
                                                           , Pred "parent" [Var "M", Var "X"]
                                                           ]
    , Rule (Pred "father" [Var "P", Var "X"])              [ Pred "man"    [Var "P"]
                                                           , Pred "parent" [Var "P", Var "X"]
                                                           ]
    , Rule (Pred "human"  [Var "X"])                       [ Pred "human"  [Var "P"]
                                                           , Pred "parent" [Var "P", Var "X"]
                                                           ]
    , Rule (Pred "human"  [Var "X"])                       [ Pred "human"  [Var "P"]
                                                           , Pred "parent" [Var "P", Var "X"]
                                                           , Pred "human"  [Var "M"]
                                                           , Pred "parent" [Var "M", Var "X"]
                                                           ]
    , Rule (Pred "siblings" [Var "X", Var "Y"])            [ Pred "parent" [Var "Z", Var "X"]
                                                           , Pred "parent" [Var "Z", Var "Y"]
                                                           ]
    ]

g11 = Pred "mother" [Pred "yolanda" [], Pred "silvia" []]
g12 = Pred "mother" [Pred "silvia" [], Pred "cristian" []]
g13 = Pred "siblings" [Pred "silvia" [], Pred "carlos" []]
g14 = Pred "mother" [Var "A", Var "B"]

p2 = 
    [ Rule (Pred "negro" [cnst "cristian"]) []
    , Rule (Pred "negro" [cnst "totoy"])    []
    , Rule (Pred "negro" [cnst "maxi"])     []
    , Rule (Pred "negro" [cnst "ruben"])    []
    , Rule (Pred "negros" [Var "X", Var "Y"]) [ Pred "negro" [Var "X"]
                                              , Pred "negro" [Var "Y"]
                                              ]
    ]

-- p2 = [ Rule (Pred "negro" [cnst "cristian"]) [], Rule (Pred "negro" [cnst "totoy"])    [], Rule (Pred "negro" [cnst "maxi"])     [], Rule (Pred "negro" [cnst "ruben"])    [], Rule (Pred "negros" [Var "X", Var "Y"]) [ Pred "negro" [Var "X"] , Pred "negro" [Var "Y"]]]

g21 = Pred "negro" [cnst "cristian"]
g22 = Pred "negro" [cnst "totoy"]
g23 = Pred "negro" [cnst "maxi"]
g24 = Pred "negro" [cnst "ruben"]
g25 = Pred "negros" [Var "A", Var "B"]