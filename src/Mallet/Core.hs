module Mallet.Core
  ( CoreTerm
  , CoreTermF
  , Identifier
  , Term(..)
  , TermF(..)
  , makeApp
  , makeBinding
  , makeType
  , makeVar
  , fold
  , unfold
  , roll
  , unroll
  )
where

import           Bound.Name
import           Data.Text
import           Mallet.Core.Term
import           Mallet.Core.TermF
import           Numeric.Natural

type CoreTermF = TermF Identifier

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
