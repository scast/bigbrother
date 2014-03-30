module Main where
import qualified Parser
import Lexer
import Table
import System.Environment
import Control.Monad.RWS hiding ((<>))
import Text.PrettyPrint
import qualified Data.Map as M

prettySymbolTable :: Table -> Doc
prettySymbolTable table
  = braces (
    imprimirSimbolos table
    $$ imprimirHijos table
    )

imprimirSimbolos table = text "SIMBOLOS => " $$ M.foldl' f empty (mapping table)
  where f a b = a $$ nest 5 (braces (imprimirSimbolo b))

imprimirSimbolo (TypeDeclaration (Parser.Ident nombre l c) table) =
  text "DECLARACION DE TIPO"
  $$ text ("nombre = " ++ nombre)
  $$ (if (l > 0 && c > 0)
     then text ("ubicacion = " ++ (show l)++":"++(show c))
     else empty)
  $$ if (M.null (mapping table)) then empty else prettySymbolTable table

imprimirSimbolo (Variable (Parser.Ident nombre l c) kind) =
  text "DECLARACION DE VARIABLE"
  $$ text ("nombre = " ++ nombre)
  $$ (if (l > 0 && c > 0)
     then text ("ubicacion = " ++ (show l)++":"++(show c))
     else empty)
  $$ text ("modificador = " ++ (show kind))

imprimirSimbolo (Function (Parser.Ident nombre l c) table) =
  text "DECLARACION DE FUNCION"
  $$ text ("nombre = " ++ nombre)
  $$ (if (l > 0 && c > 0)
     then text ("ubicacion = " ++ (show l)++":"++(show c))
     else empty)
  $$ prettySymbolTable table

imprimirHijos table =  text "HIJOS => " $$ foldl f empty (sons table)
  where f a b = a $$ nest 5 (prettySymbolTable b)

main = do
  [file] <- getArgs
  s <- readFile file
  -- run lexer
  let (errors, tokens) = lexer s
  mapM_ putStrLn errors
  -- run parser
  let Right tree = Parser.parseTokens tokens
      (st, w) = execRWS build tree (GeneratorState emptyTable [])
  mapM_ putStrLn w
  putStrLn $ render $ prettySymbolTable (current st)
