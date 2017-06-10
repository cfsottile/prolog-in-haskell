module Main where

import Datatypes (ProgramMap, Valuation)
import Prolog
import Parser
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Printing
import System.IO

main :: IO ()
main = do
    putDoc (line <> help <> line)
    main'
    
main' :: IO ()
main' = do
    cmd <- getLine
    case cmd of
        ('l':cmd') -> loadProgram $ drop 2 cmd
        _   -> putDoc (line <> help <> line) >> main'

loadProgram :: FilePath -> IO ()
loadProgram fileName = do
    preProgram <- readFile fileName
    let programMap = genMap <$> parseProgram preProgram
    either print (programLoaded fileName) programMap

programLoaded :: FilePath -> ProgramMap -> IO ()
programLoaded fileName pm = do
    putDoc (line <> queryHelp <> line)
    programReady fileName pm

programReady :: FilePath -> ProgramMap -> IO ()
programReady fileName pm = do
    putDoc $ line <> bold (text "Programa cargado: " <> brackets (yellow (text fileName))) <> line
    cmd <- getLine
    case cmd of
        ('?':cmd') -> answer fileName pm $ drop 2 cmd
        ('u':cmd') -> putDoc (line <> (bold . text) "Programa descargado" <> line) >> main'
        _          -> programReady fileName pm

answer :: FilePath -> ProgramMap -> String -> IO ()
answer fileName pm unparsedGoal =
    let Right goal = parseGoal unparsedGoal
    in if isGround goal
        then do
            putDoc $ red (showAnswerIf (askIf goal pm)) <> line
            programReady fileName pm
        else forward fileName pm $ askAll goal pm

forward :: FilePath -> ProgramMap -> [Valuation] -> IO ()
forward fileName pm [] = programReady fileName pm
forward fileName pm (v:vs) = do
    putDoc $ showOneAnswer v
    hFlush stdout
    forward' fileName pm vs
  where
    forward' fileName pm vs = do
        cmd <- getLine
        case cmd of
            (';':cmd') -> forward fileName pm vs
            ('.':cmd') -> programReady fileName pm
            _          -> putDoc (answerHelp <> line) >> forward' fileName pm vs


help :: Doc
help = bold ((underline . string) "Uso:" <> line <> string
                "• " <> (yellow . text) "l file" <> string ": cargar archivo Prolog.\n\
                \• " <> (yellow . text) "u" <> string ": descarga el programa actual.\n\
                \• " <> (yellow . text) "h" <> string ": ayuda.")

answerHelp :: Doc
answerHelp = yellow (string "\t• `;`: imprimir siguiente\n\
                            \\t• `q`: cancelar")

queryHelp :: Doc
queryHelp = bold $ (underline . string) "Consultas:" <> line <> string "• " <> (yellow . text) "? query" <>
            string ": efectuar consulta al programa cargado.\nFormato de una query/term:\n\
            \\t<query> ::= <term>.\n\
            \\t<term>  ::= <string> | <string>(<args>)\n\
            \\t<args>  ::= <term>   | <term>,<args>\n\
            \En caso de ser una consulta sin variables, la respuesta será True / False.\n\
            \En caso de ser una consulta con variables, la respuesta será la lista de\n\
            \valuaciones que satisfacen la consulta. En caso de haber más de una, se\n\
            \imprimirá la primera y se esperará por la entrada del usuario:\n"
            <> answerHelp
