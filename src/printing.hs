module Printing where

import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Map as Map

import Datatypes

showAnswerIf :: Bool -> Doc
showAnswerIf = bold . showBool
    where
        showBool True = text "Sí"
        showBool False = text "No"

showAnswerAll :: [Substitution] -> Doc
showAnswerAll = indent 4 . vcat . map showValuation

showOneAnswer :: Substitution -> Doc
showOneAnswer = indent 4 . showValuation

showValuation :: Substitution -> Doc
showValuation = brackets' . align . fillSep . punctuate comma . valuationDocs
    where
        valuationDocs = Map.foldWithKey (\v p l -> showInstantiation v p : l) []
        brackets' = enclose (text "[ ") (text " ]")

showInstantiation :: Term -> Term -> Doc
showInstantiation var pred = showTerm var <> text " = " <> showTerm pred

showTerm :: Term -> Doc
showTerm v@(Var _)    = red $ bold $ showVar  v
showTerm p@(Pred _ _) = bold $ showPred p

showVar :: Term -> Doc
showVar (Var v) = text v

showPred :: Term -> Doc
showPred (Pred name args)
    | null args = text name
    | otherwise = text name <> parens (showArgs args)

showArgs :: Args -> Doc
showArgs = hcat . punctuate comma . map showTerm

-- HTML show
showTermHtml v@(Var _)    = showVar  v
showTermHtml p@(Pred _ _) = showPredHtml p

showAnswerAllHtml = map showSubstitutionHtml

showSubstitutionHtml = brackets' . align . fillSep . punctuate comma . valuationDocs
    where
        valuationDocs = Map.foldWithKey (\v p l -> showInstantiationHtml v p : l) []
        brackets' = enclose (text "[ ") (text " ]")

showInstantiationHtml var pred = showTermHtml var <> text " = " <> showTermHtml pred

showAnswerIfHtml True  = [text "Sí"]
showAnswerIfHtml False = [text "No"]

showPredHtml (Pred name args)
    | null args = text name
    | otherwise = text name <> parens (showArgsHtml args)

showArgsHtml = hcat . punctuate comma . map showTermHtml