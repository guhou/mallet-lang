module Mallet.Core.Gen
    ( MkIdentifier(..)
    , arbitraryIdentifier
    )
where

import           Data.Char                      ( isLetter )
import qualified Data.Text                     as Text

import           Test.QuickCheck                ( Arbitrary(..)
                                                , Gen
                                                , arbitraryUnicodeChar
                                                , listOf1
                                                , suchThat
                                                )

import           Mallet.Core.Term               ( Identifier )

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier = fmap Text.pack (listOf1 arbitraryLetter)

arbitraryLetter :: Gen Char
arbitraryLetter = arbitraryUnicodeChar `suchThat` isLetter

newtype MkIdentifier = MkIdentifier Identifier deriving (Eq, Ord, Read, Show)

instance Arbitrary MkIdentifier where
    arbitrary = fmap MkIdentifier arbitraryIdentifier
