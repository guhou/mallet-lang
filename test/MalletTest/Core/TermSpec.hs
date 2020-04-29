module MalletTest.Core.TermSpec
  ( spec
  )
where

import           Bound                         as Bound
import           Bound.Name
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Poly
import           Mallet.Core
import           MalletTest.Core.Internal

spec :: Spec
spec = do
  describe "Applicative Term" $ do
    it "identity" $ property $ \(TermA v) -> (pure id <*> v) === v

    it "composition" $ property $ \(TermBC funU, TermAB funV, TermA w) ->
      let u = fmap applyFun funU
          v = fmap applyFun funV
      in  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

    it "homomorphism" $ property $ \(FunAB funF, ArgA x) ->
      let f = applyFun funF in (pure f <*> pure x) === (pure (f x) :: Term B)

    it "interchange" $ property $ \(TermAB funU, ArgA y) ->
      let u = fmap applyFun funU in (u <*> pure y) === (pure ($ y) <*> u)

  describe "makeBinding" $ do
    it "does not bind non-matching variables in body"
      $ property
      $ \(MkIdentifier x, MkIdentifier y) ->
          x /= y ==> makeBinding x (Type 0) (Var y) `shouldBe` Binding
            (Type 0)
            (Scope (Var (F (Var y))))

    it "binds matching variables in body" $ property $ \(MkIdentifier x) ->
      makeBinding x (Type 0) (Var x)
        `shouldBe` Binding (Type 0) (Scope (Var (Bound.B (Name x ()))))

  describe "makeVar"
    $ it "creates a var with a local name"
    $ property
    $ \(MkIdentifier x) -> makeVar x `shouldBe` Var x
