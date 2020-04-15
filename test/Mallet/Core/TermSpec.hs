module Mallet.Core.TermSpec
    ( spec
    )
where

import           Bound

import           Test.Hspec

import           Mallet.Core.Term

spec :: Spec
spec = describe "makeBinding" $ do
    it "does not bind non-matching variables in body"
        $          makeBinding "x" (Type 0) (Var "y")
        `shouldBe` Binding (Type 0) (Scope (Var (F (Var "y"))))
    it "binds matching variables in body"
        $          makeBinding "x" (Type 0) (Var "x")
        `shouldBe` Binding (Type 0) (Scope (Var (B ())))
