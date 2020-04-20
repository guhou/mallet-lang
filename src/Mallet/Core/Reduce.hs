module Mallet.Core.Reduce
  ( reduce
  )
where

import           Bound                          ( instantiate1 )
import           Data.Functor.Foldable          ( embed
                                                , fold
                                                )

import           Mallet.Core.Term               ( CoreTerm
                                                , CoreTermF
                                                , Term(..)
                                                , TermF(..)
                                                )

reduce :: CoreTerm -> CoreTerm
reduce = fold beta

beta :: CoreTermF CoreTerm -> CoreTerm
beta termF = case termF of
  AppF (Binding _ body) argument -> instantiate1 argument body
  _                              -> embed termF
