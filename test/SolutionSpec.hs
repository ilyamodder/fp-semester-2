module SolutionSpec where

import Test.Hspec

import Types
import Solution

spec = do
  it "typeOf" $ do
    typeOf (Lam "x" Nat (Add (Sym "x") (Natural 5))) `shouldBe` Right (Fun Nat Nat)

    typeOf (Lam "x" Bool $ Sym "x") `shouldBe` Right (Fun Bool Bool)

    typeOf (Add (Natural 5) (Boolean False)) `shouldBe` Left "second argument of Add must be nutural number"

    typeOf (App (Lam "x" Nat $ Sym "x") (Natural 5)) `shouldBe` Right Nat

    typeOf (App (Lam "x" Nat $ Boolean False) (Natural 5)) `shouldBe` Right Bool

    typeOf (App (Lam "x" Bool $ Boolean False) (Natural 5)) `shouldBe` Left "App args wrong type"

    typeOf (Nil Nat) `shouldBe` Right (List Nat)

    typeOf (Cons (Natural 5) $ Cons (Boolean False) $ Nil Nat) `shouldBe` Left "second argument of Cons must be list"