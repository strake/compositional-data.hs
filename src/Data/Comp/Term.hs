{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Data.Comp.Term where

data Cxt (h :: Bool) f a b i where
    In :: f a (Cxt h f a b) i -> Cxt h f a b i
    Hole :: b i -> Cxt True f a b i
    Var :: a i -> Cxt h f a b i

newtype Term f i = Term { unTerm :: ∀ a . Cxt False f a (Const ()) i }

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
