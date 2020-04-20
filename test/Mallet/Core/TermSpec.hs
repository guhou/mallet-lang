module Mallet.Core.TermSpec
    ( spec
    )
where

import           Bound                          ( Scope(..)
                                                , Var(..)
                                                )
import           Bound.Name                     ( Name(..) )

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.QuickCheck                ( (==>)
                                                , property
                                                )

import           Mallet.Core.Term               ( Term(..)
                                                , makeBinding
                                                , makeVar
                                                )
import           Mallet.Core.Gen                ( MkIdentifier(..) )

spec :: Spec
spec = do
    describe "makeBinding" $ do
        it "does not bind non-matching variables in body"
            $ property
            $ \(MkIdentifier x, MkIdentifier y) ->
                  x /= y ==> makeBinding x (Type 0) (Var y) `shouldBe` Binding
                      (Type 0)
                      (Scope (Var (F (Var y))))

        it "binds matching variables in body" $ property $ \(MkIdentifier x) ->
            makeBinding x (Type 0) (Var x)
                `shouldBe` Binding (Type 0) (Scope (Var (B (Name x ()))))

    describe "makeVar" $ do
        it "creates a var with a local name" $ property $ \(MkIdentifier x) ->
            makeVar x `shouldBe` Var x
