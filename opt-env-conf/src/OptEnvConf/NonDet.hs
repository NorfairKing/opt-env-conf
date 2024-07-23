{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module OptEnvConf.NonDet
  ( runNonDet,
    runNonDetT,
    runNonDetTLazy,
    liftNonDetTList,
    NonDetT,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity

type NonDet = NonDetT Identity

runNonDet :: NonDet a -> [a]
runNonDet = runIdentity . runNonDetT

type NonDetT = ListT

runNonDetT :: (Monad m) => NonDetT m a -> m [a]
runNonDetT = runListTComplete

runNonDetTLazy :: (Monad m) => NonDetT m a -> m (Maybe (a, NonDetT m a))
runNonDetTLazy = runListTLazy

liftNonDetTList :: (Applicative m) => [a] -> NonDetT m a
liftNonDetTList = liftListT

-- The monadic list type
data MList m a
  = MNil
  | MCons a (m (MList m a))

instance (Functor f) => Functor (MList f) where
  fmap f = \case
    MNil -> MNil
    MCons a r -> MCons (f a) (fmap (fmap f) r)

liftMList :: (Applicative m) => [a] -> MList m a
liftMList = \case
  [] -> MNil
  (a : as) -> MCons a $ pure $ liftMList as

joinMMList :: (Monad m) => MList m (m (MList m a)) -> m (MList m a)
joinMMList = \case
  MNil -> pure MNil
  MCons a m -> appendMMMList a (m >>= joinMMList)

joinMMMList :: (Monad m) => m (MList m (m (MList m a))) -> m (MList m a)
joinMMMList = (>>= joinMMList)

appendMList :: (Functor m) => MList m a -> MList m a -> MList m a
appendMList MNil ml = ml
appendMList (MCons a ml1) ml2 = MCons a $ (`appendMList` ml2) <$> ml1

appendMMMList :: (Applicative m) => m (MList m a) -> m (MList m a) -> m (MList m a)
appendMMMList ml1 ml2 = appendMList <$> ml1 <*> ml2

-- This can be directly used as a monad transformer
newtype ListT m a = ListT {unListT :: m (MList m a)}

-- A "lazy" run function, which only calculates the first solution.
runListTLazy :: (Functor m) => ListT m a -> m (Maybe (a, ListT m a))
runListTLazy = fmap g . unListT
  where
    g MNil = Nothing
    g (x `MCons` xs) = Just (x, ListT xs)

runListTComplete :: (Monad m) => ListT m a -> m [a]
runListTComplete = unListT >=> go
  where
    goML f = f >>= go
    go = \case
      MNil -> pure []
      MCons a f -> (a :) <$> goML f

liftListT :: (Applicative m) => [a] -> ListT m a
liftListT = ListT . pure . liftMList

instance (Functor f) => Functor (ListT f) where
  fmap f = ListT . fmap (fmap f) . unListT

instance (Monad f) => Applicative (ListT f) where
  pure a = ListT (pure (MCons a (pure MNil)))
  (<*>) ff fa = do
    f <- ff
    a <- fa
    pure (f a)

instance (MonadIO m) => MonadIO (ListT m) where
  liftIO = lift . liftIO

instance MonadTrans ListT where
  lift = ListT . fmap (`MCons` pure MNil)

-- Note: This alternative instance only "alternates" on the nondeterminism, not the
-- underlying effect.
instance (Monad f) => Alternative (ListT f) where
  empty = ListT $ pure MNil
  (<|>) (ListT l1) (ListT l2) = ListT $ appendMMMList l1 l2

instance (Monad f) => Monad (ListT f) where
  (>>=) m f = joinListT $ fmap f m

joinListT :: (Monad m) => ListT m (ListT m a) -> ListT m a
joinListT (ListT xss) = ListT . joinMMMList $ fmap (fmap unListT) xss
