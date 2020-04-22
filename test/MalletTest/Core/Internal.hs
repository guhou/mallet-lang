module MalletTest.Core.Internal where

import           Bound
import           Bound.Name
import           Bound.Scope
import           Data.Bitraversable
import           Data.Char
import qualified Data.Text                     as Text
import           Mallet.Core.Term
import           Test.QuickCheck         hiding ( function )
import           Test.QuickCheck.Instances.Natural
                                                ( )
import           Test.QuickCheck.Poly

newtype MkIdentifier = MkIdentifier Identifier deriving (Eq, Ord, Read, Show)

instance Arbitrary MkIdentifier where
    arbitrary = MkIdentifier <$> arbitraryIdentifier

newtype TermA = TermA  (Term A) deriving (Show)
instance Arbitrary TermA where
    arbitrary = fmap TermA arbitraryTerm

newtype TermB = TermB  (Term B) deriving (Show)
instance Arbitrary TermB where
    arbitrary = fmap TermB arbitraryTerm

newtype TermC = TermC  (Term C) deriving (Show)
instance Arbitrary TermC where
    arbitrary = fmap TermC arbitraryTerm

newtype TermBC = TermBC (Term (Fun B C)) deriving ( Show)
instance Arbitrary TermBC where
    arbitrary = fmap TermBC arbitraryTerm

newtype TermAB = TermAB  (Term (Fun A B)) deriving ( Show)
instance Arbitrary TermAB where
    arbitrary = fmap TermAB arbitraryTerm


newtype FunAB = FunAB  (Fun A B) deriving ( Show)
instance Arbitrary FunAB where
    arbitrary = fmap FunAB arbitrary

newtype ArgA = ArgA A deriving ( Show)
instance Arbitrary ArgA where
    arbitrary = fmap ArgA arbitrary

arbitraryCoreTerm :: Gen CoreTerm
arbitraryCoreTerm = do
    identifiers <- listOf1 arbitraryIdentifier
    let mkBinder = elements identifiers
    let mkAbstract term = do
            identifier <- mkBinder
            pure $ abstract1Name identifier term
    makeTermGen mkBinder mkAbstract

arbitraryTerm :: (Arbitrary a) => Gen (Term a)
arbitraryTerm = do
    identifiers <- listOf1 arbitraryIdentifier
    binders     <- listOf1 arbitrary
    let mkIdentifier = elements identifiers
    let mkBinder     = elements binders
    let mkAbstract term = do
            identifier <- mkIdentifier
            capture    <- arbitrary :: Gen (Maybe ())
            let boundTerm = abstract (const capture) term
            pure $ mapBound (Name identifier) boundTerm
    makeTermGen mkBinder mkAbstract

makeTermGen
    :: Gen a
    -> (Term a -> Gen (Scope (Name Identifier ()) Term a))
    -> Gen (Term a)
makeTermGen mkBinder mkAbstract =
    let termGen = do
            size <- getSize
            if size > 2
                then oneof [typeGen, varGen, appGen, bindingGen]
                else oneof [typeGen, varGen]

        typeGen = Type <$> arbitrarySizedNatural

        varGen  = Var <$> mkBinder

        appGen  = do
            (functionSize, argumentSize) <- splitSize
            App <$> resize functionSize termGen <*> resize argumentSize termGen

        bindingGen = do
            (codomainSize, bodySize) <- splitSize
            codomain                 <- resize codomainSize termGen
            bodyTerm                 <- resize bodySize termGen
            body                     <- mkAbstract bodyTerm
            pure $ Binding codomain body
    in  termGen

shrinkTerm :: Term a -> [Term a]
shrinkTerm = shrinkTermWith (const [])

shrinkTermWith :: (a -> [a]) -> Term a -> [Term a]
shrinkTermWith s term = case term of
    Type universe -> [ Type universe' | universe' <- shrink universe ]
    Var  a        -> [ Var a' | a' <- s a ]
    App function argument ->
        [function, argument]
            ++ [ App function argument'
               | argument' <- shrinkTermWith s argument
               ]
            ++ [ App function' argument
               | function' <- shrinkTermWith s function
               ]
    Binding codomain body ->
        [codomain]
            ++ [ Binding codomain' body
               | codomain' <- shrinkTermWith s codomain
               ]
            ++ [ Binding codomain body' | body' <- shrinkScope body ]
  where
    shrinkScope = fmap toScope . shrinkTermWith shrinkVar . fromScope
    shrinkVar   = bitraverse pure s

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier = fmap Text.pack (listOf1 arbitraryLetter)

arbitraryLetter :: Gen Char
arbitraryLetter = arbitraryUnicodeChar `suchThat` isLetter

splitSize :: Gen (Int, Int)
splitSize = do
    size  <- getSize
    split <- choose (0, size)
    pure (split, size - split)
