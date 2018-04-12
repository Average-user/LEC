module Main where

import Converter  (toCNF, toDNF)
import Data.Maybe (isNothing)
import Parser     (parseExpr)
import Rainbow    (Radiant, putChunk, chunk, (&), fore, red, cyan)

color :: Radiant
color = cyan

main :: IO ()
main = do
  putChunk $ chunk "\nInsert Expression:\n" & fore color
  expr <- parseExpr <$> getLine
  if isNothing expr
    then do putChunk $ chunk "\nNot a valid expression\n\n" & fore red
            main
    else do let (Just expr') = expr
            putChunk $ chunk "\nParsed Expression:       " & fore color
            putStrLn $(show expr')
            putChunk $ chunk "Conjunctive normal form: " & fore color
            print (toCNF expr')
            putChunk $ chunk "Disjunctive normal form: " & fore color
            print (toDNF expr')
            putStr "\n"
            main
