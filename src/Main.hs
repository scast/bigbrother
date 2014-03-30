module Main where
import qualified Parser
import Lexer
import Table
import System.Environment
import Control.Monad.RWS hiding ((<>))
import Text.PrettyPrint
import System.Console.ANSI
import qualified Data.Map as M
import Data.List (intercalate)

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

-- imprimirHijos table =  text "HIJOS => " $$ foldl f empty (sons table)
--   where f a b = a $$ nest 5 (prettySymbolTable b)



dropUntilFirstToTheLeft :: Int -> [Lexeme] -> (Maybe String, [Lexeme])
dropUntilFirstToTheLeft atleast tokens = (matched, newList)
  where newList = dropWhile (\x -> matchFunc (str x)) (drop atleast tokens)
        matched = if (null newList) then Nothing else Just (str (head newList))
        matchFunc x = x /= "(" && x /= "{"

toTheRight "(" = ")"
toTheRight "{" = "}"

dropUntilFirstToTheRight :: Maybe String -> Int -> [Lexeme] -> (Maybe String, [Lexeme])
dropUntilFirstToTheRight Nothing _ _ = (Nothing, [])
dropUntilFirstToTheRight (Just match) atleast tokens = (matched, newList)
  where newList = dropWhile (\x -> (str x) /= (toTheRight match)) (drop atleast tokens)
        matched = if (null newList) then Nothing else Just (str (head newList))

-- Tecnica "Dale que el golpe avisa", brought to you by dos futuros
-- IDE operators de la república.
-- Razones para esto están por ahí en un email.
tryParse :: [Lexeme] -> IO [Parser.Global]
tryParse tokens = do
  case Parser.parseTokens tokens of
    Right tree ->       setSGR [SetColor Foreground Dull Red] >> return tree
    Left (err, leftTokens) -> do
      setSGR [SetColor Foreground Vivid Red]
      let context = intercalate " " (take 3 (map str leftTokens))
      putStrLn (err ++ " cerca de \"" ++ context ++ "\"")
      let totalTokens = length tokens
          toGoTokens = length leftTokens
          splitPos = (totalTokens - toGoTokens)
          (matchLeft, newLeftTokens) = (dropUntilFirstToTheLeft 0 (reverse (take splitPos tokens) ))
          (matchRight, newRightTokens) = dropUntilFirstToTheRight matchLeft 0 leftTokens
      case (matchLeft,matchRight) of
        (Nothing, _) -> return []
        (_, Nothing) -> return []
        _ -> do
          setSGR [SetColor Foreground Vivid Yellow]
          putStrLn $ "Tratando de recuperarme..."
      -- putStrLn $ show $ take (splitPos) tokens
          putStrLn $ show newLeftTokens
          putStrLn $ show newRightTokens
          tryParse $ (reverse newLeftTokens) ++ newRightTokens

main = do
  [file] <- getArgs
  s <- readFile file
  -- run lexer
  let (errors, tokens) = lexer s
  setSGR [SetColor Foreground Vivid Red]
  mapM_ putStrLn errors
  -- run parser
  tree <- tryParse tokens
  let (st, w) = execRWS build tree (GeneratorState emptyTable [])
  setSGR [SetColor Foreground Dull White]
  mapM_ putStrLn w
  putStrLn $ render $ prettySymbolTable (current st) 0
