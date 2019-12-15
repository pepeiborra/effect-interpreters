> {-# LANGUAGE ConstraintKinds, DerivingVia, DerivingStrategies, GeneralizedNewtypeDeriving, KindSignatures, NoMonomorphismRestriction, RecordWildCards #-}
> {-# LANGUAGE GADTs, QuantifiedConstraints, RankNTypes #-}
> module Control.Monad.Interpret where
> import Control.Monad.Identity
> import Control.Monad.IO.Class
> import Control.Monad.Trans.Reader
> import Control.Monad.Trans.State
> import Control.Monad.Trans.Class
> import Control.Monad.Trans.Control
> import Data.Time.Clock (NominalDiffTime, diffUTCTime)
> import qualified Data.Time.Clock as Time



Effect interpreters
===========================

Dynamic effects
----------------------

Sometimes it can be desirable to intercept or change the behaviour of a monadic effect dynamically.
To make things concrete, let's assume an effect that allows to declare cost centers:

> class Monad m => MonadCostCenter m where
>   registerCostCenter :: Name -> m a -> m a

One possible implementation generates log lines for every start/complete event of a cost center:

> newtype ViaLogging m a = ViaLogging {runViaLogging :: m a}
>   deriving (Applicative, Functor, Monad, MonadIO) via (IdentityT m)
>   deriving MonadTrans via IdentityT

> instance MonadLog m => MonadCostCenter (ViaLogging m) where
>   registerCostCenter name action = do
>     ViaLogging $ logMsg ("Starting cost center " <> name)
>     res <- action
>     ViaLogging $ logMsg ("Completed cost center" <> name)
>     return res

Another possibility is to collect all the timings in a data structure for processing later:

> data Timing = Timing {name :: String, duration :: NominalDiffTime}

> newtype CollectTimingsT m a = CollectTimingsT (StateT [Timing] m a)
>   deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans, MonadTransControl)

> runCollectTimings :: Monad m => ([Timing] -> m ()) -> CollectTimingsT m a -> m a
> runCollectTimings doSomethingWithTimings (CollectTimingsT action) = do
>   (res, timings) <- runStateT action []
>   doSomethingWithTimings timings
>   return res

> instance MonadTime m => MonadCostCenter (CollectTimingsT m) where
>   registerCostCenter name action = do
>     startTime <- CollectTimingsT $ lift getCurrentTime
>     res <- action
>     endTime <- CollectTimingsT $ lift getCurrentTime
>     let duration = diffUTCTime endTime startTime
>     CollectTimingsT $ modify (Timing{..} :)
>     return res

Assume our application is a web service and it doesn't care to collect timings unless
explicitly required by the request being handled. Our code will look like:

> type HandlerMonad = WebT (CostCenterT (LogT (TimeT IO)))
>
> runHandler :: HandlerMonad a -> IO a
> runHandler = undefined

But what is the type `CostCenterT`? Didn't we say that it depends on the request?
Well yes, we want to handle cost centers differently depending on the request,
but the Haskell type system requires that the type of the *carrier* `HandlerMonad` is fixed.
This choice can be encoded explicitly using an `Either` based carrier:

> type HandlerMonad' = WebT (EitherT ViaLogging CollectTimingsT (LogT (TimeT IO)))
>
> newtype EitherT t1 t2 (m :: * -> *) a = EitherT {runEitherT :: Either (t1 m a) (t2 m a)}

The rest of `EitherT` boilerplate (instances, run function) are not pretty and left as an exercise for the reader.
Is there a better way?

Effect interpreters
----------------------

The problem described above does not apply to some effect systems like *polysemy*,
where there is no explicit carrier. Effect systems with explicit carriers like *fused-effects*
and *transformers* can work around this by defining an `Interpreter` transformer.
Indeed, *fused-effects* includes the monad transfomer `Control.Effect.Interpret.InterpretC s sig`
which can be used to intercept an effect `sig` implemented by the underlying monad `m`.

We can define a similar abstraction for vanilla *transformers* as follows:

> newtype InterpretT c m a = InterpretT (ReaderT (Interpreter c m) m a)
>   deriving (Applicative, Functor, Monad, MonadIO)
>
> instance MonadTrans (InterpretT c) where
>   lift = InterpretT . lift
>
> data Interpreter c (m :: * -> *) where
>   Interpreter :: c (t m) => (forall a . t m a -> m a) -> Interpreter c m
>
> runInterpretT :: Interpreter c m -> InterpretT c m a -> m a
> runInterpretT run (InterpretT action) = runReaderT action run
>
> wrapEffect :: Monad m => (forall m . c m => m a) -> InterpretT c m a
> wrapEffect action = InterpretT $ do
>   Interpreter run <- ask
>   lift (run action)

Now we can define `HandlerMonad` and `runHandler` as follows:

> type HandlerMonad'' = InterpretT MonadCostCenter (WebT (LogT (TimeT IO)))
>
> runHandler'' = runTimeT
>              . runLogT
>              . runWebT
>              . runInterpretT (if True then Interpreter runViaLogging else Interpreter (runCollectTimings sendTimings))


Dynamic effect interpreters
----------------------------

The solution above works well for simple dynamism, but sometimes we want to change
or extend the interpreter *within* the computation. Something like:

> localInterpreter :: (Interpreter c m -> Interpreter c m) -> InterpretT c m a -> InterpretT c m a
> localInterpreter f (InterpretT action) = InterpretT $ local f action

This is almost useful, except that there is no practical way to delegate to the
previous interpreter. It allows overwrite only:

> switchToCollectTimings :: ([Timing -> m ()]) -> HandlerMonad'' a -> HandlerMonad'' a
> switchToCollectTimings doTimings = localInterpreter (const $ Interpreter $ runCollectTimings sendTimings)

In order to enable delegation, we have to resort to another monad transformer:

> newtype Both (t1 :: (* -> *) -> * -> *) t2 (m :: * -> *) a = Both {runBoth :: t1 (t2 m) a}
>   deriving (Applicative, Functor, Monad, MonadIO)

> instance (forall m . Monad m => Monad (t2 m), MonadTrans t2, MonadTrans t1) => MonadTrans (Both t1 t2) where
>   lift = Both . lift . lift

> instance (MonadCostCenter (t2 m)
>          ,MonadCostCenter (t1 (t2 m))
>          ,MonadTransControl t1
>          ,Monad m
>          ) => MonadCostCenter (Both t1 t2 m) where
>   registerCostCenter name (Both action) = Both
>     $ registerCostCenter name
>     $ liftWith (\runInT2 -> registerCostCenter name (runInT2 action)) >>= restoreT . return

Now we can write the function below:

> class (MonadTime m, MonadCostCenter m) => MonadCostCenterTime m
> instance (MonadTime m, MonadCostCenter m) => MonadCostCenterTime m

> type HandlerMonad''' = InterpretT MonadCostCenterTime (WebT (LogT (TimeT IO)))

> addTimingsCollection :: (forall m . MonadTime m => [Timing]-> m ()) -> HandlerMonad''' a -> HandlerMonad''' a
> addTimingsCollection doTimings = localInterpreter $ \(Interpreter delegate) ->
>    Interpreter (delegate . runCollectTimings doTimings . runBoth)

Can we do better, is there an approach that enables delegation without resorting to an additional monad transformer?

Placeholders
-------------

> type CostCenterT = IdentityT
> type LogT = IdentityT
> type TimeT = IdentityT
> type WebT = IdentityT
> type Name = String
> type MonadLog  = Monad
> type MonadTime = MonadIO
> runTimeT = runIdentityT
> runLogT  = runIdentityT
> runWebT  = runIdentityT
> logMsg = return
> getCurrentTime = liftIO Time.getCurrentTime
> sendTimings _ = return ()
