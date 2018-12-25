module Data.Comp.Equality where

import Control.Applicative
import Control.Category.Const2
import Control.Monad.Trans.Fresh
import Data.Eq

class PEq a where
    peq :: a i -> a j -> Fresh Bool

instance Eq a => PEq (Const a) where
    peq (Const x) (Const y) = pure (x == y)

instance PEq Name where
    peq x y = pure (map (Const2 ()) x == y)

class EqH f where
    eqH :: PEq a => f Name a i -> f Name a j -> Fresh Bool
