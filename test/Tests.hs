{-# LANGUAGE RecordWildCards #-}

import Converter            (toCNF, toDNF)
import Cases                (CaseA(..), CaseB(..), casesA, casesB)
import Control.Monad.Reader (Reader(..), asks, runReader)
import Data.Foldable        (for_)
import Data.List            (nub, sort)
import Data.Maybe           (fromJust)
import Parser               (parseExpr)
import Test.Hspec           (Spec, describe, it, shouldBe)
import Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck      (Arbitrary(..), quickCheckWith, maxSuccess, maxSize, choose, sized, stdArgs)
import Types                (Expr(..), symbol)
import Syntax               (goodCNF, goodDNF)

main :: IO ()
main = do
  putStr "\n"
  quickCheckWith (stdArgs {maxSize = 7, maxSuccess = 500}) testCNF
  quickCheckWith (stdArgs {maxSize = 7, maxSuccess = 500}) testDNF
  hspecWith defaultConfig {configFastFail = True} specs
  putStrLn "\n"

specs :: Spec
specs = do describe "CNF Tests" $ for_ casesA testA
           describe "Parser Tests" $ for_ casesB testB
  where
    testA CaseA{..} = it descriptionA (show . toCNF <$> parseExpr inputA `shouldBe` expectedA)
    testB CaseB{..} = it descriptionB (parseExpr inputB `shouldBe` expectedB)

eval :: Expr -> Reader [(String, Bool)] Bool
eval (Var x)   = asks (lookup x) >>= maybe (fail $ "Var `" ++ x ++ "` not defined!") return
eval (Not x)   = not <$> eval x
eval (a :&: b) = (&&) <$> eval a <*> eval b
eval (a :|: b) = (||) <$> eval a <*> eval b
eval (a :+: b) = (/=) <$> eval a <*> eval b
eval (a :=: b) = (==) <$> eval a <*> eval b
eval (a :>: b) = (\a b -> not a || b) <$> eval a <*> eval b

fullImage :: Expr -> [Bool]
fullImage e = (runReader (eval e) . zip vs) <$> bits (length vs)
  where
    vs             = nub $ sort $ vars e
    vars (Var x)   = [x]
    vars (Not x)   = vars x
    vars (a :&: b) = vars a ++ vars b
    vars (a :|: b) = vars a ++ vars b
    vars (a :=: b) = vars a ++ vars b
    vars (a :>: b) = vars a ++ vars b
    vars (a :+: b) = vars a ++ vars b
    bits 0         = [[]]
    bits n         = ((False :) <$> xs) ++ ((True:) <$> xs)
      where
        xs = bits (n - 1)

testCNF :: Expr -> Bool
testCNF = ((&&) . goodCNF . show <*> ((==) . fullImage <*> fullImage)) . toCNF

testDNF :: Expr -> Bool
testDNF = ((&&) . goodDNF . show <*> ((==) . fullImage <*> fullImage)) . toDNF

instance Arbitrary Expr where
  arbitrary = sized gen
    where
      gen n = choose (1, max 1 (min 7 n)) >>= f
        where
          f 1 = Var . (:"") . (['a'..]!!) <$> choose (1, 8)
          f 2 = (:&:) <$> gen (n-1) <*> gen (n-1)
          f 3 = (:|:) <$> gen (n-1) <*> gen (n-1)
          f 4 = (:>:) <$> gen (n-1) <*> gen (n-1)
          f 5 = (:=:) <$> gen (n-1) <*> gen (n-1)
          f 6 = (:+:) <$> gen (n-1) <*> gen (n-1)
          f 7 = Not <$> gen (n-1)
