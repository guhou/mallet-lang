module MalletTest.Core.Internal
    ( MkCoreTerm(..)
    , MkIdentifier(..)
    , MkTerm(..)
    , arbitraryCoreTerm
    , arbitraryIdentifier
    , arbitraryTerm
    , hspecLaws
    )
where

import           Bound
import           Bound.Name
import           Bound.Scope
import           Data.Bitraversable
import           Data.Char
import qualified Data.Text                     as Text
import           Mallet.Core.Term
import           Test.Hspec
import           Test.QuickCheck         hiding ( function )
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Instances.Natural
                                                ( )
import           Test.QuickCheck.Instances.Text ( )

newtype MkIdentifier = MkIdentifier { unIdentifier :: Identifier } deriving (Eq, Ord, Read, Show)

instance Arbitrary MkIdentifier where
    arbitrary = fmap MkIdentifier arbitraryIdentifier
    shrink    = fmap MkIdentifier . shrink . unIdentifier

newtype MkTerm a = MkTerm { unMkTerm :: Term a } deriving (Eq, Ord, Read, Show)

instance Arbitrary a => Arbitrary (MkTerm a) where
    arbitrary = fmap MkTerm arbitraryTerm
    shrink    = fmap MkTerm . shrinkTerm . unMkTerm

instance Applicative MkTerm where
    pure = MkTerm . pure
    f <*> x = MkTerm (unMkTerm f <*> unMkTerm x)

instance Foldable MkTerm where
    foldMap f = foldMap f . unMkTerm

instance Functor MkTerm where
    fmap f = MkTerm . fmap f . unMkTerm

instance Monad MkTerm where
    x >>= f = MkTerm (unMkTerm x >>= unMkTerm . f)

instance Traversable MkTerm where
    traverse f = fmap MkTerm . traverse f . unMkTerm

newtype MkCoreTerm = MkCoreTerm { unMkCoreTerm :: CoreTerm } deriving (Eq, Ord, Read, Show)

instance Arbitrary MkCoreTerm where
    arbitrary = fmap MkCoreTerm arbitraryCoreTerm
    shrink    = fmap MkCoreTerm . shrinkTerm . unMkCoreTerm

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

shrinkTerm :: Arbitrary a => Term a -> [Term a]
shrinkTerm = liftShrinkTerm shrink

liftShrinkTerm :: (a -> [a]) -> Term a -> [Term a]
liftShrinkTerm shrinkValue term = case term of
    Type universe -> [ Type universe' | universe' <- shrink universe ]
    Var  value    -> [ Var value' | value' <- shrinkValue value ]
    App function argument ->
        [function, argument]
            ++ [ App function argument' | argument' <- shrinkTerm' argument ]
            ++ [ App function' argument | function' <- shrinkTerm' function ]
    Binding codomain body ->
        [codomain]
            ++ [ Binding codomain' body | codomain' <- shrinkTerm' codomain ]
            ++ [ Binding codomain body' | body' <- shrinkScope body ]
  where
    shrinkTerm' = liftShrinkTerm shrinkValue
    shrinkScope = fmap toScope . liftShrinkTerm shrinkVar . fromScope
    shrinkVar   = bitraverse pure shrinkValue

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier = fmap Text.pack (listOf1 arbitraryLetter)

arbitraryLetter :: Gen Char
arbitraryLetter = arbitraryUnicodeChar `suchThat` isLetter

splitSize :: Gen (Int, Int)
splitSize = do
    size  <- getSize
    split <- choose (0, size)
    pure (split, size - split)

hspecLaws :: Laws -> Spec
hspecLaws laws = describe
    (lawsTypeclass laws)
    (mapM_ (parallel . uncurry it) (lawsProperties laws))
