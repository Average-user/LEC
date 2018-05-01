module Syntax (goodCNF, goodDNF) where

import Parser (validExpression, varChars)

goodCNF :: String -> Bool
goodCNF e = and [ validExpression e
                , all (`notElem` ">=+") e
                , depth e `elem` [0,1]
                , okStructure
                ]
  where
    inside     = words $ insideBrackets e
    outside    = words $ outsideBrackets e
    properties = [ all ((||) . (=="|") <*> validVar) outside
                 , all ((||) . (=="&") <*> validVar) outside
                 , all ((||) . (=="|") <*> validVar) inside
                 ]
    okStructure | depth e == 0 = properties!!0 || properties!!1
                | otherwise    = properties!!1 && properties!!2

goodDNF :: String -> Bool
goodDNF e = and [ validExpression e
                , all (`notElem` ">=+") e
                , depth e `elem` [0,1]
                , okStructure
                ]
  where
    inside     = words $ insideBrackets e
    outside    = words $ outsideBrackets e
    properties = [ all ((||) . (=="|") <*> validVar) outside
                 , all ((||) . (=="&") <*> validVar) outside
                 , all ((||) . (=="&") <*> validVar) inside
                 ]
    okStructure | depth e == 0 = properties!!0 || properties!!1
                | otherwise    = properties!!2 && properties!!0

depth :: String -> Int
depth = f [] 0
  where
    f r c []       = maximum (c : r)
    f r c ('(':xs) = f (c : r) (c+1) xs
    f r c (')':xs) = f (c : r) (c-1) xs
    f r c (_:xs)   = f r c xs

insideBrackets :: String -> String
insideBrackets = f False
  where
    f _     []       = []
    f True  (')':xs) = ' ' : f False xs 
    f False ('(':xs) = f True xs
    f True  (x:xs)   = x : f True xs
    f False (_:xs)   = f False xs

outsideBrackets :: String -> String
outsideBrackets = f True
  where
    f _     []       = []
    f False (')':xs) = ' ' : f True xs 
    f True  ('(':xs) = f False xs
    f True  (x:xs)   = x : f True xs
    f False (_:xs)   = f False xs

validVar :: String -> Bool
validVar "!"     = False
validVar ('!':v) = all (`elem` varChars) v && head v /= '_'
validVar v       = validVar ('!':v)
