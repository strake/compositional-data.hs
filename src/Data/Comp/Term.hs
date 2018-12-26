{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Comp.Term where

import "base" Prelude (($), Show (..), Eq (..), Applicative (..))
import Unsafe.Coerce

import Control.Monad.Trans.Fresh
import Data.Comp.Equality
import Data.Comp.Show

data Cxt (h :: Bool) f a b i where
    In :: f a (Cxt h f a b) i -> Cxt h f a b i
    Hole :: b i -> Cxt True f a b i
    Var :: a i -> Cxt h f a b i

newtype Term f i = Term { unTerm :: ∀ a . Trm f a i }

type Trm f a = Cxt False f a (Const ())

toCxt :: Bifunctor (Dual (NT (->))) (NT (->)) (NT (->)) f => Trm f a i -> Cxt h f a b i
toCxt = unsafeCoerce

instance Functor (NT (->)) (NT (->)) (f a) => Functor (NT (->)) (NT (->)) (Cxt h f a) where
    map f = NT (\ case
            Hole x -> Hole (nt f x)
            Var  v -> Var v
            In   t -> In (nt (map (map f :: NT (->) _ _)) t))

instance (Bifunctor (NT (->)) (NT (->)) (NT (->)) f) =>
         Functor (NT (->)) (NT (NT (->))) (Cxt h f) where
    map f = NT (bimap f (id @(NT (->))))

instance Bifunctor (NT (->)) (NT (->)) (NT (->)) f =>
         Bifunctor (NT (->)) (NT (->)) (NT (->)) (Cxt h f) where
    bimap f g = NT (\ case
        Hole x -> Hole (nt g x)
        Var  v -> Var (nt f v)
        In   t -> In (nt (bimap f (bimap f g :: NT (->) _ _)) t))

instance EqH f => EqH (Cxt h f) where
    eqH (In s) (In t) = eqH s t
    eqH (Hole x) (Hole y) = peq x y
    eqH (Var u) (Var v) = peq u v
    eqH _ _ = pure False

instance (EqH f, PEq a) => PEq (Cxt h f Name a) where
    peq = eqH

instance EqH f => Eq (Term f i) where
    Term x == Term y = evalFresh (eqH x y)

instance (Bifunctor (Dual (NT (->))) (NT (->)) (NT (->)) f, ShowH f) => ShowH (Cxt h f) where
    showH = \ case
        In   t -> showH (bimap (Dual (id @(NT (->)))) (NT (Const . showH)) `nt` t)
        Var  v -> pure $ show v
        Hole x -> getConst x

instance (Bifunctor (Dual (NT (->))) (NT (->)) (NT (->)) f, ShowH f) => Show (Term f i) where
    show = evalFresh . showH . toCxt . unTerm
