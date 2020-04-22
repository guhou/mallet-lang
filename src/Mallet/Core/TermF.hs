module Mallet.Core.TermF
  ( Algebra
  , Coalgebra
  , TermF(..)
  , fold
  , unfold
  , roll
  , unroll
  )
where

import           Bound
import           Bound.Name
import           Control.Applicative
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Functor.Classes
import           Data.Text                      ( Text )
import           Mallet.Core.Term
import           Numeric.Natural
import           Text.Read

type Algebra b a = TermF b a -> a

type Coalgebra b a = a -> TermF b a

unfold :: Coalgebra b a -> a -> Term b
unfold g = a where a = roll . fmap a . g

fold :: Algebra b a -> Term b -> a
fold f = c where c = f . fmap c . unroll

roll :: TermF b (Term b) -> Term b
roll termF = case termF of
  TypeF universe             -> Type universe
  VarF  identifier           -> Var identifier
  AppF     function argument -> App function argument
  BindingF codomain body     -> Binding codomain body

unroll :: Term b -> TermF b (Term b)
unroll term = case term of
  Type universe             -> TypeF universe
  Var  identifier           -> VarF identifier
  App     function argument -> AppF function argument
  Binding codomain body     -> BindingF codomain body

data TermF b a
    = TypeF Natural
    | VarF b
    | AppF a a
    | BindingF a (Scope (Name Text ()) Term b)

instance Bifoldable TermF where
  bifoldMap f g termF = case termF of
    TypeF{}                    -> mempty
    VarF identifier            -> f identifier
    AppF     function argument -> mappend (g function) (g argument)
    BindingF codomain body     -> mappend (g codomain) (foldMap f body)

instance Bifunctor TermF where
  bimap f g termF = case termF of
    TypeF universe             -> TypeF universe
    VarF  identifier           -> VarF (f identifier)
    AppF     function argument -> AppF (g function) (g argument)
    BindingF codomain body     -> BindingF (g codomain) (fmap f body)

instance Bitraversable TermF where
  bitraverse onB onA termF = case termF of
    TypeF universe  -> pure $ TypeF universe
    VarF  b         -> VarF <$> onB b
    AppF     a a'   -> AppF <$> onA a <*> onA a'
    BindingF a body -> BindingF <$> onA a <*> traverse onB body

instance (Eq b, Eq a) => Eq (TermF b a) where
  (==) = eq2

instance (Eq b) => Eq1 (TermF b) where
  liftEq = liftEq2 (==)

instance Eq2 TermF where
  liftEq2 _ _ (TypeF universeA) (TypeF universeB) = universeA == universeB

  liftEq2 beq _ (VarF identifierA) (VarF identifierB) =
    beq identifierA identifierB

  liftEq2 _ aeq (AppF functionA argumentA) (AppF functionB argumentB) =
    aeq functionA functionB && aeq argumentA argumentB

  liftEq2 beq aeq (BindingF codomainA bodyA) (BindingF codomainB bodyB) =
    aeq codomainA codomainB && liftEq beq bodyA bodyB

  liftEq2 _ _ _ _ = False

instance Functor (TermF b) where
  fmap = bimap id

instance Foldable (TermF b) where
  foldMap = bifoldMap (const mempty)

instance (Read b, Read a) => Read (TermF b a) where
  readPrec = readPrec2

instance Read b => Read1 (TermF b) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

instance Read2 TermF where
  liftReadPrec2 rpb rlb rpa _ =
    readData
      $   readUnaryWith readPrec "TypeF" TypeF
      <|> readUnaryWith rpb      "VarF"  VarF
      <|> readBinaryWith rpa rpa                    "AppF"     AppF
      <|> readBinaryWith rpa (liftReadPrec rpb rlb) "BindingF" BindingF

instance (Show b, Show a) => Show (TermF b a) where
  showsPrec = showsPrec2

instance (Show b) => Show1 (TermF b) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 TermF where
  liftShowsPrec2 spb slb spa _ p term = case term of
    TypeF universe   -> showsUnaryWith showsPrec "TypeF" p universe

    VarF  identifier -> showsUnaryWith spb "VarF" p identifier

    AppF function argument ->
      showsBinaryWith spa spa "AppF" p function argument

    BindingF codomain body ->
      showsBinaryWith spa (liftShowsPrec spb slb) "BindingF" p codomain body

instance Traversable (TermF b) where
  traverse = bitraverse pure
