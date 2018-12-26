module Data.Comp.Show where

import Control.Monad.Trans.Fresh

class ShowH f where
    showH :: f Name (Const (Fresh [Char])) i -> Fresh [Char]
