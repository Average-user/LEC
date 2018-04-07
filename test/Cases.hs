module Cases where

import Types (Expr(..))

casesA :: [CaseA]
casesA = [ CaseA { descriptionA = "CNF Test"
                 , inputA       = "(a & b) | (c & d)"
                 , expectedA    =  Just "(a ∨ c) ∧ (a ∨ d) ∧ (b ∨ c) ∧ (b ∨ d)"
                 }
         ]

casesB :: [CaseB]
casesB = [ CaseB { descriptionB = "Variables must start with a letter"
                 , inputB       = "_1"
                 , expectedB    = Nothing
                 },
           CaseB { descriptionB = "literal is a valid expresion"
                 , inputB       = "a1"
                 , expectedB    = Just $ Var "a1"
                 },
           CaseB { descriptionB = "Parentheses should match"
                 , inputB       = "(a | b))"
                 , expectedB    = Nothing
                 },
           CaseB { descriptionB = "Simple disjunction"
                 , inputB       = "(a | b)"
                 , expectedB    = Just $ Var "a" :|: Var "b"
                 },
           CaseB { descriptionB = "More complex expression"
                 , inputB       = "((a | b) > !(b + c)) = c"
                 , expectedB    = Just $ ((Var "a" :|: Var "b") :>: Not (Var "b" :+: Var "c")) :=: Var "c"
                 },
           CaseB { descriptionB = "Exp"
                 , inputB       = "(a & b) | (c & d)"
                 , expectedB    = Just $ ((Var "a") :&: (Var "b")) :|: ((Var "c") :&: (Var "d"))
                 }
         ]

-- For testing formulas that I KNOW that are correctly written
data CaseA = CaseA { descriptionA :: String
                   , inputA       :: String
                   , expectedA    :: Maybe String
                   }

-- For testing fromulas that could or could not be correctly written
data CaseB = CaseB { descriptionB :: String
                   , inputB       :: String
                   , expectedB    :: Maybe Expr
                   }
