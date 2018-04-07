module Parser (parseExpr) where

import Data.Either                        (isRight)
import Data.Functor                       (($>))
import Data.Functor.Identity              (Identity)
import Data.List                          ((\\))
import Data.Maybe                         (isJust)
import Text.Parsec.Prim                   (ParsecT)
import Text.ParserCombinators.Parsec      (GenParser, parse, (<?>), (<|>), char, oneOf, many1)
import Text.ParserCombinators.Parsec.Expr (Operator(..), Assoc(..), buildExpressionParser)
import Types                              (Expr(..))

parseExpr :: String -> Maybe Expr
parseExpr exp = if validExpresion exp && isRight parsed
                  then let Right x = parse expr "blah" exp' in Just x
                  else Nothing
  where
    exp'   = filter (/=' ') exp
    parsed = parse expr "blah" exp'

expr :: ParsecT String u Identity Expr
expr = buildExpressionParser table factor <?> "expression"

table :: [[Operator Char st Expr]]
table = [ [Prefix (char '!' $> Not)]
        , [Infix (char '|' $> (:|:)) AssocLeft]
        , [Infix (char '&' $> (:&:)) AssocLeft]
        , [Infix (char '=' $> (:=:)) AssocLeft]
        , [Infix (char '>' $> (:>:)) AssocNone]
        , [Infix (char '+' $> (:+:)) AssocLeft]
        ]

atomP :: ParsecT String u Identity Expr
atomP = Var <$> many1 (oneOf varChars)

factor :: GenParser Char u Expr
factor = do { char '(' ; x <- expr ; char ')' ; return x }
   <|> atomP
   <?> "simple expression"

validExpresion :: String -> Bool
validExpresion = and . sequence [ pairedBrackets
                                , all (`elem` allowedChars)
                                , validVariableNames
                                ]
  where
    allowedChars = varChars ++ "!&|>=+() "

pairedBrackets :: String -> Bool
pairedBrackets =
  (== Just []) . last . takeWhile isJust . iterate f . Just . normalize
  where
    normalize = filter (`elem` "()")
    f (Just [])           = Nothing
    f (Just ('(':')':xs)) = Just xs
    f (Just (x:xs))       = (x:) <$> f (Just xs)

validVariableNames :: String -> Bool
validVariableNames xs = all validName names
  where
    ws          = words $ (\x -> if x `elem` "()" then ' ' else x) <$> xs
    variables   = filter (`notElem` ["&","|","!",">","+","="]) ws
    names       = (\x -> if head x == '!' then tail x else x) <$> variables
    validName x = head x `elem` (varChars \\ "_") && all (`elem` varChars) x

varChars :: String
varChars = '_' : ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
