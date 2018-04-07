module Converter (toCNF, toDNF) where

import Types (Expr(..))

toCNF :: Expr -> Expr
toCNF e = if e == e' then e else toCNF e' 
  where e' = f e 
        f x@(Var _)         = x 
        f x@(Not (Var _))   = x 
        f (Not (Not x))     = x 
        f (Not (a :|: b))   = f (Not a :&: Not b)
        f (Not (a :&: b))   = f (Not a :|: Not b)
        f ((a :&: b) :|: c) = f ((a :|: c) :&: (b :|: c))
        f (a :|: (b :&: c)) = f ((a :|: b) :&: (a :|: c))
        f (a :&: b)         = f a :&: f b
        f (a :|: b)         = f a :|: f b
        f (a :>: b)         = f (Not a) :|: f b
        f (a :=: b)         = f (a :&: b) :|: f ((Not a) :&: (Not b))
        f (a :+: b)         = f ((Not a) :|: (Not b)) :&: f (a :|: b)
        f (Not (a :>: b))   = f (a :&: (Not b))
        f (Not (a :=: b))   = f (a :+: b)
        f (Not (a :+: b))   = f (a :=: b)

toDNF :: Expr -> Expr
toDNF e = if e == e' then e else toDNF e' 
  where e' = f e 
        f x@(Var _)         = x 
        f x@(Not (Var _))   = x 
        f (Not (Not x))     = x 
        f (Not (a :|: b))   = f (Not a :&: Not b)
        f (Not (a :&: b))   = f (Not a :|: Not b)
        f ((a :|: b) :&: c) = f ((a :&: c) :|: (b :&: c))
        f (a :&: (b :|: c)) = f ((a :&: b) :|: (a :&: c))
        f (a :&: b)         = f a :&: f b
        f (a :|: b)         = f a :|: f b
        f (a :>: b)         = f (Not a) :|: f b
        f (a :=: b)         = f (a :&: b) :|: f (Not a :&: Not b)
        f (a :+: b)         = f (Not a :|: Not b) :&: f (a :|: b)
        f (Not (a :>: b))   = f (a :&: Not b)
        f (Not (a :=: b))   = f (a :+: b)
        f (Not (a :+: b))   = f (a :=: b)
