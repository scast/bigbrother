module Main where
-- import Parser
import Lexer



tokens =  snd $ lexer "static x, y, z : int;"

main = do
  putStrLn $ show $ parser $ tokens
