module Main where
import qualified Parser
import Lexer
import Control.Monad.Writer
import Table
import System.Environment
import Control.Monad.RWS hiding ((<>))
import Text.PrettyPrint
import System.Console.ANSI
import qualified Data.Map as M
import Data.List (intercalate)
import Control.Monad.Trans.Either

-- prettySymbolTable :: Table -> Int -> Doc
-- prettySymbolTable table ubic
--   = (text "{")
--   $$ (imprimirSimbolos table (ubic+1))
--   $$ (imprimirHijos table (ubic+1))
--   $$ (text "}")

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
  -- $$ if (M.null (mapping table)) then empty else prettySymbolTable table

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
  -- $$ prettySymbolTable table

imprimirHijos table =  text "HIJOS => " $$ foldl f empty (sons table)
  where f a b = a $$ nest 5 (prettySymbolTable b)

dropUntilFirstToTheLeft :: Int -> [Lexeme] -> (Maybe String, [Lexeme])
dropUntilFirstToTheLeft atleast tokens = (matched, newList)
  where newList = dropWhile (\x -> matchFunc (str x)) (drop atleast tokens)
        matched = if (null newList) then Nothing else Just (str (head newList))
        matchFunc x = x /= "(" && x /= "{"

toTheRight "(" = ")"
toTheRight "{" = "}"

dropUntilFirstToTheRight :: Maybe String -> Int -> [Lexeme] -> (Maybe String, [Lexeme])
dropUntilFirstToTheRight Nothing _ _ = (Nothing, [])
dropUntilFirstToTheRight (Just ";") atleast tokens = (matched, if shouldDrop
                                                               then take 1 newList
                                                               else newList)
  where newList = dropWhile (\x -> (str x) /= "}" && (str x) /= ";") (drop atleast tokens)
        matched = if (null newList)
                  then Nothing
                  else Just (str (head newList))
        shouldDrop =
          case matched of
            Just ";" -> True
            _ -> False

dropUntilFirstToTheRight (Just match) atleast tokens = (matched, newList)
  where newList = dropWhile (\x -> (str x) /= (toTheRight match)) (drop atleast tokens)
        matched = if (null newList) then Nothing else Just (str (head newList))

-- Tecnica "Dale que el golpe avisa", brought to you by dos futuros
-- IDE operators de la república.
-- Razones para esto están por ahí en un email.
tryParse :: [Lexeme] -> IO [Parser.Global]
tryParse tokens = do
  case runWriter (runEitherT (Parser.parseTokens tokens)) of
    (eitherTree, []) -> case eitherTree of
      -- Gaceta hipica, no hubo errores y parseamos bebe.
      Right tree -> setSGR [SetColor Foreground Dull Red] >> return tree
      -- Hubo errores no recuperables, y de paso no parsee.
      Left (err, leftTokens) -> tryParse' tokens leftTokens err
    (eitherTree, errors) -> case eitherTree of
      -- there were errors but parsed. gaceta hipica.
      Right tree ->   setSGR [SetColor Foreground Vivid Red] >> mapM_ putStrLn errors >> return tree
      -- there were errors and nothing was parsed...
      Left (err, leftTokens) ->   setSGR [SetColor Foreground Vivid Red] >> mapM_ putStrLn errors >> return []

tryParse' tokens leftTokens err = do
    -- Left (err, leftTokens) -> do
  setSGR [SetColor Foreground Vivid Red]
  let context = intercalate " " (take 3 (map str leftTokens))
  putStrLn (err ++ " cerca de \"" ++ context ++ "\""
            ++ (if (null leftTokens)
                then ""
                else " (" ++ (showPosn (pos (leftTokens !! 0))) ++ ")"))

  let totalTokens = length tokens
      toGoTokens = length leftTokens
      splitPos = (totalTokens - toGoTokens)
      workingLeft = (reverse (take splitPos tokens) )
      atleast = case (workingLeft, leftTokens) of
        ([], _) -> 0
        (_, []) -> 0
        ((x:xs), (y:ys)) ->
          case ((str x), (str y)) of
            ("{", "}") -> 1
            ("(", ")") -> 1
            _ -> 0
      (matchLeft, newLeftTokens) = (dropUntilFirstToTheLeft atleast workingLeft)
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
          -- ... se hace lo que se puede con lo que se tiene...
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

  putStrLn $ render $ prettySymbolTable (current st)
