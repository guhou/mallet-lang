{-# LANGUAGE OverloadedStrings #-}
module Mallet.Core.ParseSpec
    ( spec
    )
where

import           Bound                          ( Scope(..)
                                                , Var(..)
                                                )
import           Bound.Name                     ( Name(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Text.Megaparsec                ( parse )
import           TextShow                       ( showt )

import           Test.Hspec                     ( Expectation
                                                , Spec
                                                , context
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Megaparsec          ( shouldFailOn
                                                , shouldParse
                                                )
import           Test.QuickCheck                ( property )
import           Test.QuickCheck.Instances.Natural
                                                ( )

import           Mallet.Core.Parse              ( parseTerm )
import           Mallet.Core.Gen                ( MkIdentifier(..) )
import           Mallet.Core.Term               ( CoreTerm
                                                , Term(..)
                                                )

spec :: Spec
spec = describe "parseTerm" $ do
    context "empty input"
        $              it "fails on empty input"
        $              parse parseTerm ""
        `shouldFailOn` ""

    context "applications" $ do
        it "parses applications of constants"
            $ property
            $ \(MkIdentifier f, MkIdentifier x) ->
                  let stream = Text.unwords [f, x]
                  in  stream `shouldParseTerm` App (Var f) (Var x)

        it "parses Type applied to constants" $ property $ \(MkIdentifier x) ->
            let stream = Text.unwords ["Type", x]
            in  stream `shouldParseTerm` App (Type 0) (Var x)

        it "parses constants applied to Type" $ property $ \(MkIdentifier f) ->
            Text.unwords [f, "Type"] `shouldParseTerm` App (Var f) (Type 0)

    context "bindings" $ do
        it "parses bindings with constants"
            $ let stream = "\\(x : Type), Type"
              in  stream `shouldParseTerm` Binding (Type 0) (Scope (Type 0))

        it "parses bindings with free variables"
            $ let stream = "\\(x : Int), Z"
              in  stream `shouldParseTerm` Binding
                      (Var "Int")
                      (Scope (Var (F (Var "Z"))))

        it "parses bindings with bound variables"
            $ let stream = "\\(x : Int), x"
              in  stream `shouldParseTerm` Binding
                      (Var "Int")
                      (Scope (Var (B (Name "x" ()))))

    context "Type" $ do
        it "can parse Type with implicit universe"
            $                 "Type"
            `shouldParseTerm` Type 0
        it "can parse Type with universe parameter" $ property $ \universe ->
            let stream = Text.unwords ["Type", showt universe]
            in  stream `shouldParseTerm` Type universe

    context "Var"
        $ it "can parse identifiers"
        $ property
        $ \(MkIdentifier identifier) ->
              identifier `shouldParseTerm` Var identifier

shouldParseTerm :: Text -> CoreTerm -> Expectation
shouldParseTerm stream term = parse parseTerm "" stream `shouldParse` term
