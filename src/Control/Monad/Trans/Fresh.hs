module Control.Monad.Trans.Fresh (FreshT, evalFreshT, Fresh, evalFresh, withName, Name) where

import "base" Prelude (Show (..), (+), flip, ($), Eq (..))
import qualified "base" Data.Functor as Hs

import Control.Applicative (Applicative (..), Alternative (..))
import Control.Monad (Monad (..), MonadPlus (..))
import Control.Monad.Trans.Reader
import qualified Data.List as List (genericIndex, reverse)

newtype FreshT m a = FreshT { unFreshT :: ReaderT Natural m a }
  deriving newtype (Hs.Functor, Applicative, Monad, Alternative, MonadPlus)

newtype Name i = Name Natural
  deriving (Eq)

instance Category s => Functor s (->) Name where
    map _ (Name n) = Name n

instance Show (Name i) where
    show (Name n) = List.genericIndex names n
      where names = map List.reverse $ flip (:) <$> "":names <*> (['a'..'z'] <|> ['α'..'ω'])

evalFreshT :: FreshT m a -> m a
evalFreshT = flip runReaderT 0 . unFreshT

type Fresh = FreshT Identity

evalFresh :: Fresh a -> a
evalFresh = runIdentity . evalFreshT

withName :: Monad m => (Name i -> FreshT m a) -> FreshT m a
withName f = FreshT (asks Name) >>= FreshT . local (+1) . unFreshT . f
