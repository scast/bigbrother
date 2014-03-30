module Main where
import Parser
import Lexer
import Table
import Control.Monad.RWS


tokens =  snd $ lexer "static x, y, z : int;"

globals = parseTokens $ snd $ lexer "type jojojo = int; fn fun():jojojo { if (x > 0 ) { print! X; } }"
buildTable (Right globals) = execRWS build globals (GeneratorState emptyTable [])

main = do
  putStrLn "Hola"
