{-|
module: Mallet.Core.Term
description: The Mallet Core language
copyright: (c) Angus Houston 2020
license: GPL-3.0-or-later
maintainer: angus.houston@outlook.com.au
stability: experimental

Term represents the expression language for Mallet Core. It is a dependently-
typed lambda calculus similar to the Calculus of Constructions.
-}
module Mallet.Core.Term
    ( CoreTerm
    , CoreTermF
    , Identifier
    , Term(..)
    , TermF(..)
    , makeApp
    , makeBinding
    , makeType
    , makeVar
    )
where

import           Bound.Name                     ( abstract1Name )
import           Data.Text                      ( Text )
import           Numeric.Natural                ( Natural )

import           Mallet.Core.Internal

-- | The type of Core terms with text identifiers
type CoreTerm = Term Identifier

-- | The base functor of the CoreTerm type- can be used to write recursion
-- schemes on the CoreTerm type in terms of base functor algebrae and
-- coalgebrae.
type CoreTermF = TermF Identifier

-- | Identifiers for Core variables and binders
type Identifier = Text

-- | construct a CoreTerm by applying the given CoreTerms together. Note that
-- this constructor is simply a type-narrowed version of the Core App
-- constructor.
makeApp :: CoreTerm -> CoreTerm -> CoreTerm
makeApp = App

-- | construct a CoreTerm binder using the specified binder name, binding
-- codomain, and body. This is a smart constructor that abstracts all instances
-- of the given identifier inside body. It does not abstract variables in the
-- codomain.
makeBinding :: Text -> CoreTerm -> CoreTerm -> CoreTerm
makeBinding identifier codomain body =
    Binding codomain (abstract1Name identifier body)

-- | construct a Type term in the specified universe. Note that this
-- constructor is simply a type-narrowed version of the Core Type constructor.
makeType :: Natural -> CoreTerm
makeType = Type

-- | construct a free Var term with the specified name. Note that this
-- constructor is simply a type-narrowed version of the Core Var constructor.
makeVar :: Text -> CoreTerm
makeVar = Var
