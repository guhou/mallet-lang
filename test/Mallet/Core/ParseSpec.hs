{-# LANGUAGE OverloadedStrings #-}
module Mallet.Core.ParseSpec
    ( spec
    )
where

import           Bound
import           Data.Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Natural
                                                ( )
import           Text.Megaparsec
import           TextShow

import           Mallet.Core.Parse
import           Mallet.Core.Term

spec :: Spec
spec = describe "parseTerm" $ do
    context "empty input"
        $              it "fails on empty input"
        $              parse parseTerm ""
        `shouldFailOn` ""

    context "applications" $ do
        it "parses applications of constants"
            $ property
            $ \(Identifier function, Identifier argument) ->
                  Text.unwords [function, argument]
                      `shouldParseTerm` App (Var function) (Var argument)
        it "parses Type applied to constants"
            $ property
            $ \(Identifier argument) ->
                  Text.unwords ["Type", argument]
                      `shouldParseTerm` App (Type 0) (Var argument)
        it "parses constants applied to Type"
            $ property
            $ \(Identifier function) ->
                  Text.unwords [function, "Type"]
                      `shouldParseTerm` App (Var function) (Type 0)

    context "bindings" $ do
        it "parses bindings with constants"
            $                 "\\(x : Type), Type"
            `shouldParseTerm` Binding (Type 0) (Scope (Type 0))
        it "parses bindings with free variables"
            $                 "\\(x : Int), Z"
            `shouldParseTerm` Binding (Var "Int") (Scope (Var (F (Var "Z"))))
        it "parses bindings with bound variables"
            $                 "\\(x : Int), x"
            `shouldParseTerm` Binding (Var "Int") (Scope (Var (B ())))

    context "Type" $ do
        it "can parse Type with implicit universe"
            $                 "Type"
            `shouldParseTerm` Type 0
        it "can parse Type with universe parameter"
            $ property
            $ \(NonNegative universe) -> Text.unwords ["Type", showt universe]
                  `shouldParseTerm` Type universe

    context "Var"
        $ it "can parse identifiers"
        $ property
        $ \(Identifier ident) -> ident `shouldParseTerm` Var ident

newtype Identifier = Identifier Text deriving (Show)

instance Arbitrary Identifier where
    arbitrary = Identifier . Text.pack <$> listOf1
        (arbitraryUnicodeChar `suchThat` isLetter)

shouldParseTerm :: Text -> Term Text -> Expectation
shouldParseTerm stream term = parse parseTerm "" stream `shouldParse` term
