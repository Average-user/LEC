module Types where

import Data.Maybe (fromJust)

data Expr = Var String
          | Not Expr
          | Expr :|: Expr
          | Expr :&: Expr
          | Expr :>: Expr
          | Expr :=: Expr
          | Expr :+: Expr deriving Eq

symbol :: Char -> String
symbol = fromJust . flip lookup
  [ ('!', "!")
  , ('|', " | ")
  , ('&', " & ")
  , ('>', " > ")
  , ('+', " + ")
  , ('=', " = ")
  ]

instance Show Expr where
  show (Var x)       = x 
  show (Not (Var x)) = symbol '!' ++ x 
  show (Not x)       = symbol '!' ++ "(" ++ show x ++ ")" 

  show (a :|: b)     = let show' c@(_ :&: _) = "(" ++ show c ++ ")"
                           show' c@(_ :=: _) = "(" ++ show c ++ ")"
                           show' c@(_ :>: _) = "(" ++ show c ++ ")"
                           show' c@(_ :+: _) = "(" ++ show c ++ ")"
                           show' c = show c
                         in show' a ++ symbol '|' ++ show' b

  show (a :&: b)     = let show' c@(_ :|: _) = "(" ++ show c ++ ")"
                           show' c@(_ :=: _) = "(" ++ show c ++ ")"
                           show' c@(_ :>: _) = "(" ++ show c ++ ")"
                           show' c@(_ :+: _) = "(" ++ show c ++ ")"
                           show' c = show c
                         in show' a ++ symbol '&' ++ show' b

  show (a :=: b)     = let show' c@(_ :&: _) = "(" ++ show c ++ ")"
                           show' c@(_ :|: _) = "(" ++ show c ++ ")"
                           show' c@(_ :>: _) = "(" ++ show c ++ ")"
                           show' c@(_ :+: _) = "(" ++ show c ++ ")"
                           show' c = show c
                         in show' a ++ symbol '=' ++ show' b

  show (a :+: b)     = let show' c@(_ :&: _) = "(" ++ show c ++ ")"
                           show' c@(_ :=: _) = "(" ++ show c ++ ")"
                           show' c@(_ :>: _) = "(" ++ show c ++ ")"
                           show' c@(_ :|: _) = "(" ++ show c ++ ")"
                           show' c = show c
                         in show' a ++ symbol '+' ++ show' b

  show (a :>: b)     = let show' c@(_ :&: _) = "(" ++ show c ++ ")"
                           show' c@(_ :=: _) = "(" ++ show c ++ ")"
                           show' c@(_ :>: _) = "(" ++ show c ++ ")"
                           show' c@(_ :|: _) = "(" ++ show c ++ ")"
                           show' c@(_ :+: _) = "(" ++ show c ++ ")"
                           show' c = show c
                         in show' a ++ symbol '>' ++ show' b
