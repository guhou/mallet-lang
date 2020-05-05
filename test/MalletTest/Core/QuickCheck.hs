module MalletTest.Core.QuickCheck
    ( MkCoreTerm(..)
    , MkIdentifier(..)
    , MkTerm(..)
    , arbitraryCoreTerm
    , arbitraryIdentifier
    , arbitraryTerm
    )
where

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
import           Test.QuickCheck.Instances.Text ( )

newtype MkIdentifier = MkIdentifier
    { unIdentifier :: Identifier
    } deriving (Eq, Ord, Read, Show)

instance Arbitrary MkIdentifier where
    arbitrary = fmap MkIdentifier arbitraryIdentifier
    shrink    = fmap MkIdentifier . shrink . unIdentifier

newtype MkTerm a = MkTerm
    { unMkTerm :: Term a
    } deriving (Eq, Ord, Read, Show)

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

newtype MkCoreTerm = MkCoreTerm
    { unMkCoreTerm :: CoreTerm
    } deriving (Eq, Ord, Read, Show)

instance Arbitrary MkCoreTerm where
    arbitrary = fmap MkCoreTerm arbitraryCoreTerm
    shrink    = fmap MkCoreTerm . shrinkTerm . unMkCoreTerm

arbitraryCoreTerm :: Gen CoreTerm
arbitraryCoreTerm = do
    identifiers <- listOf1 arbitraryIdentifier
    let valueGen = elements identifiers
    let makeAbstractGen term = do
            identifier <- valueGen
            let boundTerm = abstract1Name identifier term
            pure boundTerm
    makeTermGen valueGen makeAbstractGen

arbitraryTerm :: Arbitrary a => Gen (Term a)
arbitraryTerm = do
    identifiers <- listOf1 arbitraryIdentifier
    values      <- listOf1 arbitrary
    let identifierGen = elements identifiers
    let valueGen      = elements values
    let makeAbstractGen term = do
            identifier <- identifierGen
            capture    <- arbitrary
            let boundTerm = abstract (const capture) term
            let namedTerm = mapBound (Name identifier) boundTerm
            pure namedTerm
    makeTermGen valueGen makeAbstractGen

makeTermGen
    :: Gen a
    -> (Term a -> Gen (Scope (Name Identifier ()) Term a))
    -> Gen (Term a)
makeTermGen valueGen makeAbstractGen = termGen
  where
    termGen = do
        size <- getSize
        if size > 2
            then oneof [typeGen, varGen, appGen, bindingGen]
            else oneof [typeGen, varGen]

    typeGen = fmap Type arbitrarySizedNatural

    varGen  = fmap Var valueGen

    appGen  = do
        (functionSize, argumentSize) <- splitSize
        function                     <- resize functionSize termGen
        argument                     <- resize argumentSize termGen
        pure (App function argument)

    bindingGen = do
        (codomainSize, bodySize) <- splitSize
        codomain                 <- resize codomainSize termGen
        bodyTerm                 <- resize bodySize termGen
        body                     <- makeAbstractGen bodyTerm
        pure (Binding codomain body)


shrinkTerm :: Arbitrary a => Term a -> [Term a]
shrinkTerm = liftShrinkTerm shrink

liftShrinkTerm :: (a -> [a]) -> Term a -> [Term a]
liftShrinkTerm shrinkValue term = case term of
    Type universe -> fmap Type (shrink universe)

    Var  value    -> fmap Var (shrinkValue value)

    App function argument ->
        let
            subTerms        = [function, argument]
            shrinkArguments = fmap (App function) (shrinkTerm' argument)
            shrinkFunctions = fmap (flip App argument) (shrinkTerm' function)
        in
            concat [subTerms, shrinkFunctions, shrinkArguments]

    Binding codomain body ->
        let
            subTerms       = [codomain]
            shrinkBodies   = fmap (Binding codomain) (shrinkScope body)
            shrinkCodomain = fmap (flip Binding body) (shrinkTerm' codomain)
        in
            concat [subTerms, shrinkCodomain, shrinkBodies]

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
