module Main where
import qualified Parser
import Lexer
import Table
import System.Environment
import Control.Monad.RWS hiding ((<>))
import Text.PrettyPrint
import qualified Data.Map as M


prettySymbolTable :: Table -> Int -> Doc
prettySymbolTable table ubic
  = (text "{")
  $$ (imprimirSimbolos table (ubic+1))
  $$ (imprimirHijos table (ubic+1))
  $$ (text "}")

imprimirSimbolos table ubic = text "Symbols:" $$ M.foldl' f empty (mapping table)
  where f a b = a $$ nest 3 (imprimirSimbolo b ubic)

imprimirHijos table ubic =
  if (ubic==2) then empty
    else text "Sons:" $$ foldl f empty (sons table)
      where f a b = a $$ nest 3 (prettySymbolTable b ubic)

imprimirSimbolo (TypeDeclaration (Parser.Ident nombre l c) table) ubic =
  text "Type => "
  <> text (nombre ++ " ")
  <> if (l > 0 && c > 0)
     then brackets (text ((show l) ++ ":" ++ (show c)))
     else empty
  $$ if (M.null (mapping table)) then empty else (prettySymbolTable table ubic)

imprimirSimbolo (Variable (Parser.Ident nombre l c) kind) _ =
  text "Variable => "
  <> text (nombre ++ " ")
  <> (if (l > 0 && c > 0)
     then brackets (text ((show l) ++ ":" ++ (show c)))
     else empty)
  <> text (" " ++ show kind)

imprimirSimbolo (Function (Parser.Ident nombre l c) table) ubic =
  text "Function => "
  <> text (nombre ++ " ")
  <> (if (l > 0 && c > 0)
     then brackets (text ((show l) ++ ":" ++ (show c)))
     else empty)
  $$ (prettySymbolTable table ubic)

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
  putStrLn $ render $ prettySymbolTable (current st) 0
