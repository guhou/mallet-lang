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
  liftEq _  (Type universeA  ) (Type universeB  ) = universeA == universeB

  liftEq eq (Var  identifierA) (Var  identifierB) = eq identifierA identifierB

  liftEq eq (App functionA argumentA) (App functionB argumentB) =
    liftEq eq functionA functionB && liftEq eq argumentA argumentB

  liftEq eq (Binding codomainA bodyA) (Binding codomainB bodyB) =
    liftEq eq codomainA codomainB && liftEq eq bodyA bodyB

  liftEq _ _ _ = False

instance Foldable Term where
  foldMap f term = case term of
    Type _                    -> mempty
    Var  universe             -> f universe
    App function argument -> mappend (foldMap f function) (foldMap f argument)
    Binding codomain body     -> mappend (foldMap f codomain) (foldMap f body)

instance Functor Term where
  fmap f term = term >>= return . f

instance Hashable a => Hashable (Term a) where
  hashWithSalt = hashWithSalt1

instance Hashable1 Term where
  liftHashWithSalt hashA s term = case term of
    Type universe   -> s `hashWithSalt` (0 :: Int) `hashWithSalt` universe
    Var  identifier -> s `hashWithSalt` (1 :: Int) `hashA` identifier
    App function argument ->
      let hashTerm = liftHashWithSalt hashA
      in  s `hashWithSalt` (2 :: Int) `hashTerm` function `hashTerm` argument
    Binding codomain binding ->
      let hashTerm  = liftHashWithSalt hashA
          hashScope = liftHashWithSalt hashA
      in  s `hashWithSalt` (3 :: Int) `hashTerm` codomain `hashScope` binding

instance Monad Term where
  return = Var
  term >>= f = case term of
    Type universe             -> Type universe
    Var  identifier           -> f identifier
    App     function argument -> App (function >>= f) (argument >>= f)
    Binding codomain body     -> Binding (codomain >>= f) (body >>>= f)

instance NFData a => NFData (Term a) where
  rnf = rnf1

instance NFData1 Term where
  liftRnf f term = case term of
    Type universe         -> rnf universe

    Var  identifier       -> f identifier

    App function argument -> liftRnf f function `seq` liftRnf f argument

    Binding codomain body ->
      liftRnf f codomain `seq` liftRnf (unvar rnf (liftRnf f)) (unscope body)

instance Ord a => Ord (Term a) where
  compare = compare1

instance Ord1 Term where
  liftCompare _ (Type aUniverse) (Type bUniverse) = compare aUniverse bUniverse

  liftCompare cmp (Var aIdentifier) (Var bIdentifier) =
    cmp aIdentifier bIdentifier

  liftCompare cmp (App aFunction aArgument) (App bFunction bArgument) =
    liftCompare cmp aFunction bFunction <> liftCompare cmp aArgument bArgument

  liftCompare cmp (Binding aCodomain aBody) (Binding bCodomain bBody) =
    liftCompare cmp aCodomain bCodomain <> liftCompare cmp aBody bBody

  liftCompare _ Type{}    _ = LT
  liftCompare _ Var{}     _ = LT
  liftCompare _ App{}     _ = LT
  liftCompare _ Binding{} _ = GT

instance Read a => Read (Term a) where
  readPrec = readPrec1

instance Read1 Term where
  liftReadPrec rp rl =
    readData
      $   readUnaryWith readPrec "Type" Type

      <|> readUnaryWith rp       "Var"  Var

      <|> readBinaryWith (liftReadPrec rp rl) (liftReadPrec rp rl) "App" App

      <|> readBinaryWith (liftReadPrec rp rl)
                         (liftReadPrec rp rl)
                         "Binding"
                         Binding

instance Show a => Show (Term a) where
  showsPrec = showsPrec1

instance Show1 Term where
  liftShowsPrec sp sl p term = case term of
    Type universe         -> showsUnaryWith showsPrec "Type" p universe

    Var  identifier       -> showsUnaryWith sp "Var" p identifier

    App function argument -> showsBinaryWith (liftShowsPrec sp sl)
                                             (liftShowsPrec sp sl)
                                             "App"
                                             p
                                             function
                                             argument

    Binding codomain body -> showsBinaryWith (liftShowsPrec sp sl)
                                             (liftShowsPrec sp sl)
                                             "Binding"
                                             p
                                             codomain
                                             body

instance Traversable Term where
  traverse f term = case term of
    Type universe   -> pure (Type universe)
    Var  identifier -> fmap Var (f identifier)
    App function argument ->
      liftA2 App (traverse f function) (traverse f argument)
    Binding codomain body ->
      liftA2 Binding (traverse f codomain) (traverse f body)
