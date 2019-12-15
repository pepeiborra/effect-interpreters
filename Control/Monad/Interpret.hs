{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Interpret where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- | A monadic transformer for encoding an effect interpreter.
newtype InterpretT c m a = InterpretT (ReaderT (Interpreter c m) m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance MonadTrans (InterpretT c) where
  lift = InterpretT . lift

-- | An existential wrapper for a monadic "run" function
data Interpreter c (m :: * -> *) where
  Interpreter :: c (t m) => (forall a . t m a -> m a) -> Interpreter c m

-- | Run an interpreted effect with the provided interpreter
--
--   runInterpretedReader env = runInterpretT (Interpreter $ flip runReaderT env)
runInterpretT :: Interpreter c m -> InterpretT c m a -> m a
runInterpretT run (InterpretT action) = runReaderT action run

-- | > instance MonadReader (InterpretT Reader) where
--   >   ask = wrapEffect ask
wrapEffect :: Monad m => (forall t . c (t m) => t m a) -> InterpretT c m a
wrapEffect action = InterpretT $ do
   Interpreter run <- ask
   lift (run action)

-- | > instance MonadReader (InterpretT Reader) where
--   >   local f action = wrapEffectWith $ \run -> local f (run action)
wrapEffectWith :: (Monad m, c m) => ((forall a . InterpretT c m a -> m a) -> m a) -> InterpretT c m a
wrapEffectWith action = InterpretT $ do
   i@(Interpreter run) <- ask
   lift (action (runInterpretT i))

-- | A helper to encode monad transformer delegation
newtype Both (t1 :: (* -> *) -> * -> *) t2 (m :: * -> *) a = Both {runBoth :: t1 (t2 m) a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (forall m . Monad m => Monad (t2 m), MonadTrans t2, MonadTrans t1) => MonadTrans (Both t1 t2) where
  lift = Both . lift . lift
