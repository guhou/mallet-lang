module MalletTest.Core.TermSpec
  ( spec
  )
where

import           Bound
import           Bound.Name
import           Data.Proxy
import           Mallet.Core
import           MalletTest.Core.QuickCheck
import           MalletTest.Hspec
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Classes

spec :: Spec
spec = do
  hspecLaws (applicativeLaws termProxy)
  hspecLaws (eqLaws coreTermProxy)
  hspecLaws (foldableLaws termProxy)
  hspecLaws (functorLaws termProxy)
  hspecLaws (monadLaws termProxy)
  hspecLaws (ordLaws coreTermProxy)
  hspecLaws (showLaws coreTermProxy)
  hspecLaws (showReadLaws coreTermProxy)
  hspecLaws (traversableLaws termProxy)

  describe "makeApp"
    $ prop "constructs a term application"
    $ \(MkCoreTerm f, MkCoreTerm x) ->
        let expected = App f x
            actual   = makeApp f x
        in  actual === expected

  describe "makeBinding" $ do
    prop "does not bind non-matching variables in body"
      $ \(MkIdentifier x, MkIdentifier y) ->
          let expected = Binding (Type 0) (Scope (Var (F (Var y))))
              actual   = makeBinding x (Type 0) (Var y)
          in  (x /= y) ==> actual === expected

    prop "binds matching variables in body" $ \(MkIdentifier x) ->
      let expected = Binding (Type 0) (Scope (Var (B (Name x ()))))
          actual   = makeBinding x (Type 0) (Var x)
      in  actual === expected

  describe "makeType"
    $ prop "constructs a type term"
    $ \(NonNegative universe) ->
        let expected = Type universe
            actual   = makeType universe
        in  actual === expected

  describe "makeVar"
    $ prop "creates a var with a local name"
    $ \(MkIdentifier x) ->
        let expected = Var x
            actual   = makeVar x
        in  actual === expected

termProxy :: Proxy MkTerm
termProxy = Proxy

coreTermProxy :: Proxy MkCoreTerm
coreTermProxy = Proxy
