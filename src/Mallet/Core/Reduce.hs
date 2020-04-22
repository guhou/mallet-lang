module Mallet.Core.Reduce
  ( reduce
  )
where

import           Bound
import           Mallet.Core

reduce :: CoreTerm -> CoreTerm
reduce = fold beta

beta :: CoreTermF CoreTerm -> CoreTerm
beta termF = case termF of
  AppF (Binding _ body) argument -> instantiate1 argument body
  _                              -> roll termF
