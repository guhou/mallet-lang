{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Mallet.Core.Term
    ( Term(..)
    , TermF(..)
    , makeBinding
    )
where

import           Bound                          ( Scope
                                                , abstract1
                                                , makeBound
                                                )
import           Control.DeepSeq                ( NFData )
import           Data.Deriving                  ( deriveEq1
                                                , deriveRead1
                                                , deriveShow1
                                                )
import           Data.Functor.Classes           ( eq1
                                                , readsPrec1
                                                , showsPrec1
                                                )
import           Data.Functor.Foldable.TH       ( makeBaseFunctor )
import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Numeric.Natural                ( Natural )

-- Term represents terms of the Core language, and implements a
-- dependently-typed lambda calculus similar to the Calculus of Constructions.
-- Term is indexed by the type of bindings.
data Term a

    -- | Type indicates the type of terms in the core language. To avoid
    -- impredicativity, a type cannot contain itself; instead a hierarchy of
    -- type universes is created, denoted by a universe index.
    -- `Type`, written also as `Type 0`, is the type of constants, and `Type 1`
    -- is the type of `Type 0`, etc.
    -- Universe polymorphism and cumulativity is not yet implemented.
    = Type Natural

    -- | Var represents a named variable in a term. Vars may either be free
    -- variables referring to constants, or they may be variables bound by
    -- a containing Binding.
    | Var a

    -- | App represents the application of a binding term to another term:
    -- typically the application of a function to its argument, or the
    -- instantiation of a dependent product with a particular type.
    | App (Term a) (Term a)

    -- | Binding represents a named binding that is either a lambda abstraction
    -- or a dependent product type.
    -- A binding represents a term `PI(x:A), B(x)`, where `x` is a variable
    -- name, `A` is a term indicating the type of `x`, and `B(x)` is a term
    -- that may refer to the bound value of `x`.
    | Binding (Term a) (Scope () Term a)

    deriving (Foldable, Functor, Generic, Generic1, Traversable)

makeBaseFunctor ''Term
makeBound ''Term
deriveEq1  ''Term
deriveRead1 ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where
    (==) = eq1

instance NFData a => NFData (Term a)

instance Read a => Read (Term a) where
    readsPrec = readsPrec1

instance Show a => Show (Term a) where
    showsPrec = showsPrec1

makeBinding :: Eq a => a -> Term a -> Term a -> Term a
makeBinding var domain body = Binding domain (abstract1 var body)
