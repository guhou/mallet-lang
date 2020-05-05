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
  , Identifier
  , Term(..)
  )
where

import           Bound
import           Bound.Name
import           Bound.Var
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Hashable.Lifted
import           Data.Text                      ( Text )
import           Numeric.Natural
import           Text.Read

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
    | Binding (Term a) (Scope (Name Text ()) Term a)

type Identifier = Text

type CoreTerm = Term Identifier

instance Applicative Term where
  pure  = return
  (<*>) = ap

instance Eq a => Eq (Term a) where
  (==) = eq1

instance Eq1 Term where
  liftEq eqValue aTerm bTerm = case aTerm of
    Type aUniverse -> case bTerm of
      Type bUniverse -> aUniverse == bUniverse
      _              -> False

    Var aValue -> case bTerm of
      Var bValue -> eqValue aValue bValue
      _          -> False

    App aFunction aArgument -> case bTerm of
      App bFunction bArgument ->
        eqTerm aFunction bFunction && eqTerm aArgument bArgument
      _ -> False

    Binding aCodomain aBody -> case bTerm of
      Binding bCodomain bBody ->
        eqTerm aCodomain bCodomain && eqScope aBody bBody
      _ -> False

   where
    eqTerm  = liftEq eqValue
    eqScope = liftEq eqValue

instance Foldable Term where
  foldMap f term = case term of
    Type _                    -> mempty
    Var  value                -> f value
    App     function argument -> foldTerm function `mappend` foldTerm argument
    Binding codomain body     -> foldTerm codomain `mappend` foldScope body
   where
    foldTerm  = foldMap f
    foldScope = foldMap f

instance Functor Term where
  fmap f term = term >>= return . f

instance Hashable a => Hashable (Term a) where
  hashWithSalt = hashWithSalt1

instance Hashable1 Term where
  liftHashWithSalt hashValue salt term = case term of
    Type universe -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` universe

    Var  value    -> salt `hashWithSalt` (1 :: Int) `hashValue` value

    App function argument ->
      salt `hashWithSalt` (2 :: Int) `hashTerm` function `hashTerm` argument

    Binding codomain binding ->
      salt `hashWithSalt` (3 :: Int) `hashTerm` codomain `hashScope` binding

   where
    hashTerm  = liftHashWithSalt hashValue
    hashScope = liftHashWithSalt hashValue

instance Monad Term where
  return = Var
  term >>= f = case term of
    Type universe             -> Type universe
    Var  value                -> f value
    App     function argument -> App (function >>= f) (argument >>= f)
    Binding codomain body     -> Binding (codomain >>= f) (body >>>= f)

instance NFData a => NFData (Term a) where
  rnf = rnf1

instance NFData1 Term where
  liftRnf rnfValue term = case term of
    Type universe             -> rnf universe

    Var  value                -> rnfValue value

    App     function argument -> rnfTerm function `seq` rnfTerm argument

    Binding codomain body     -> rnfTerm codomain `seq` rnfScope body

   where
    rnfTerm  = liftRnf rnfValue
    rnfScope = liftRnf rnfVar . unscope
    rnfVar   = unvar rnf rnfTerm

instance Ord a => Ord (Term a) where
  compare = compare1

instance Ord1 Term where
  liftCompare compareValue aTerm bTerm = case aTerm of
    Type aUniverse -> case bTerm of
      Type bUniverse -> compare aUniverse bUniverse
      Var{}          -> LT
      App{}          -> LT
      Binding{}      -> LT

    Var aValue -> case bTerm of
      Type{}     -> GT
      Var bValue -> compareValue aValue bValue
      App{}      -> LT
      Binding{}  -> LT

    App aFunction aArgument -> case bTerm of
      Type{} -> GT
      Var{}  -> GT
      App bFunction bArgument ->
        compareTerm aFunction bFunction <> compareTerm aArgument bArgument
      Binding{} -> LT

    Binding aCodomain aBody -> case bTerm of
      Type{} -> GT
      Var{}  -> GT
      App{}  -> GT
      Binding bCodomain bBody ->
        compareTerm aCodomain bCodomain <> compareScope aBody bBody

   where
    compareTerm  = liftCompare compareValue
    compareScope = liftCompare compareValue

instance Read a => Read (Term a) where
  readPrec = readPrec1

instance Read1 Term where
  liftReadPrec readPrecValue readListPrecValue = readData $ asum
    [ readUnaryWith readPrec      "Type" Type
    , readUnaryWith readPrecValue "Var"  Var
    , readBinaryWith readPrecTerm readPrecTerm  "App"     App
    , readBinaryWith readPrecTerm readPrecScope "Binding" Binding
    ]

   where
    readPrecTerm  = liftReadPrec readPrecValue readListPrecValue
    readPrecScope = liftReadPrec readPrecValue readListPrecValue

instance Show a => Show (Term a) where
  showsPrec = showsPrec1

instance Show1 Term where
  liftShowsPrec showsPrecValue showListValue p term = case term of
    Type universe -> showsUnaryWith showsPrec "Type" p universe

    Var  value    -> showsUnaryWith showsPrecValue "Var" p value

    App function argument ->
      showsBinaryWith showsPrecTerm showsPrecTerm "App" p function argument

    Binding codomain body ->
      showsBinaryWith showsPrecTerm showsPrecScope "Binding" p codomain body

   where
    showsPrecTerm  = liftShowsPrec showsPrecValue showListValue
    showsPrecScope = liftShowsPrec showsPrecValue showListValue

instance Traversable Term where
  traverse f term = case term of
    Type universe -> pure (Type universe)

    Var  value    -> fmap Var (f value)

    App function argument ->
      liftA2 App (traverseTerm function) (traverseTerm argument)

    Binding codomain body ->
      liftA2 Binding (traverseTerm codomain) (traverseScope body)

   where
    traverseTerm  = traverse f
    traverseScope = traverse f
