{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Win32.Host
  ( Win32Guest (..),
    WriteDoUndo,
    tellDoUndo,
    DoUndo (..),
    host,
    Win32GuestConstraints,
    GuestEnv (..),
  )
where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Chan as CCC
import qualified Control.Concurrent.STM.TChan as CTC
import qualified Control.Exception as CE
import Control.Monad (forever, join, unless, when)
import qualified Control.Monad.Fix as CMF
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Primitive as CMP
import qualified Control.Monad.Reader as CMR
import Control.Monad.Ref as Ref (MonadRef, Ref, newRef, readRef, writeRef)
import qualified Control.Monad.STM as STM
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Writer as CMW
import Data.Dependent.Sum ((==>), DSum ((:=>)))
import qualified Data.Foldable as DF
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Maybe as DM
import qualified Data.Traversable as DT
import qualified Graphics.Win32 as WG
import qualified Reflex as R
import qualified Reflex.Host.Class as RH
import qualified Reflex.Spider.Internal as RI
import qualified Reflex.TriggerEvent.Base as RT
import Prelude hiding (print, putStrLn)

newtype DoUndo t = DoUndo (RH.HostFrame t (), RH.HostFrame t ())

instance Monad (RH.HostFrame t) => Semigroup (DoUndo t) where
  DoUndo (do1, undo1) <> DoUndo (do2, undo2) =
    DoUndo (do1 >> do2, undo2 >> undo1)

instance Monad (RH.HostFrame t) => Monoid (DoUndo t) where
  mempty = DoUndo (return (), return ())

type Win32GuestConstraints t (m :: * -> *) =
  ( RH.MonadReflexHost t m,
    R.MonadHold t m,
    R.MonadSample t m,
    Ref m ~ Ref IO,
    MonadRef (RH.HostFrame t),
    Ref (RH.HostFrame t) ~ Ref IO,
    MonadIO (RH.HostFrame t),
    CMP.PrimMonad (RH.HostFrame t),
    MonadIO m,
    CMF.MonadFix m
  )

newtype GuestEnv = GuestEnv {geEventLoop :: IO ()}

class (Monad m, RH.ReflexHost t) => WriteDoUndo t m where
  tellDoUndo :: DoUndo t -> m ()

instance WriteDoUndo t m => WriteDoUndo t (CMR.ReaderT a m) where
  tellDoUndo = lift . tellDoUndo

newtype Win32Guest t (m :: * -> *) a = Win32Guest
  { unWin32Guest ::
      CMW.WriterT
        (DoUndo t)
        ( CMR.ReaderT
            GuestEnv
            (R.PostBuildT t (R.TriggerEventT t (R.PerformEventT t m)))
        )
        a
  }
  deriving (Functor)

deriving instance (RH.ReflexHost t) => Applicative (Win32Guest t m)

deriving instance (RH.ReflexHost t) => Monad (Win32Guest t m)

deriving instance (RH.ReflexHost t) => CMF.MonadFix (Win32Guest t m)

deriving instance (RH.ReflexHost t, MonadRef (RH.HostFrame t)) => Ref.MonadRef (Win32Guest t m)

instance RH.ReflexHost t => WriteDoUndo t (Win32Guest t m) where
  tellDoUndo = Win32Guest . CMW.tell

instance (RH.ReflexHost t, MonadIO (RH.HostFrame t)) => MonadIO (Win32Guest t m) where
  {-# INLINEABLE liftIO #-}
  liftIO = Win32Guest . liftIO

instance (RH.ReflexHost t) => R.MonadSample t (Win32Guest t m) where
  {-# INLINEABLE sample #-}
  sample = Win32Guest . lift . R.sample

instance (RH.ReflexHost t, R.MonadHold t m) => R.MonadHold t (Win32Guest t m) where
  {-# INLINEABLE hold #-}
  hold v0 = Win32Guest . lift . R.hold v0

  {-# INLINEABLE holdDyn #-}
  holdDyn v0 = Win32Guest . lift . R.holdDyn v0

  {-# INLINEABLE holdIncremental #-}
  holdIncremental v0 = Win32Guest . lift . R.holdIncremental v0

  {-# INLINEABLE buildDynamic #-}
  buildDynamic a0 = Win32Guest . lift . R.buildDynamic a0

  {-# INLINEABLE headE #-}
  headE = Win32Guest . lift . R.headE

instance (RH.ReflexHost t) => R.PostBuild t (Win32Guest t m) where
  {-# INLINEABLE getPostBuild #-}
  getPostBuild = Win32Guest $ lift R.getPostBuild

instance
  ( RH.ReflexHost t,
    MonadRef (RH.HostFrame t),
    Ref (RH.HostFrame t) ~ Ref IO
  ) =>
  R.TriggerEvent t (Win32Guest t m)
  where
  {-# INLINEABLE newTriggerEvent #-}
  newTriggerEvent = Win32Guest $ lift R.newTriggerEvent

  {-# INLINEABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete =
    Win32Guest $ lift R.newTriggerEventWithOnComplete

  {-# INLINEABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete =
    Win32Guest . lift . R.newEventWithLazyTriggerWithOnComplete

instance
  ( RH.ReflexHost t,
    Ref m ~ Ref IO,
    MonadRef (RH.HostFrame t),
    Ref (RH.HostFrame t) ~ Ref IO,
    MonadIO (RH.HostFrame t),
    CMP.PrimMonad (RH.HostFrame t),
    MonadIO m
  ) =>
  R.PerformEvent t (Win32Guest t m)
  where
  type Performable (Win32Guest t m) = RH.HostFrame t

  {-# INLINEABLE performEvent_ #-}
  performEvent_ = Win32Guest . lift . lift . R.performEvent_

  {-# INLINEABLE performEvent #-}
  performEvent = Win32Guest . lift . lift . R.performEvent

instance
  ( RH.ReflexHost t,
    Ref m ~ Ref IO,
    R.MonadHold t m,
    CMP.PrimMonad (RH.HostFrame t),
    MonadRef (RH.HostFrame t),
    Ref (RH.HostFrame t) ~ Ref.Ref IO
  ) =>
  R.Adjustable t (Win32Guest t m)
  where
  {-# INLINEABLE runWithReplace #-}
  runWithReplace ma0 ema' = Win32Guest $ do
    ((a, DoUndo (do_, undo)), eb) <-
      lift $
        R.runWithReplace
          (CMW.runWriterT . unWin32Guest $ ma0)
          (CMW.runWriterT . unWin32Guest <$> ema')
    refUndo <- Ref.newRef undo
    CMW.tell $ DoUndo (do_, Ref.readRef refUndo & join)
    lift $ lift $ R.performEvent_ $
      (snd <$> eb) <&> \(DoUndo (newDo, newUndo)) -> do
        oldUndo <- Ref.readRef refUndo
        Ref.writeRef refUndo newUndo
        oldUndo
        newDo
    return (a, fst <$> eb)

  {-# INLINEABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust _f _dm0 _dm' = Win32Guest undefined

  --traverseIntMapWithKeyWithAdjust (\k v -> unWin32Guest (f k v)) dm0 dm'

  {-# INLINEABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust _f _dm0 _dm' = Win32Guest undefined

  --traverseDMapWithKeyWithAdjust (\k v -> unWin32Guest (f k v)) dm0 dm'

  {-# INLINEABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove _f _dm0 _dm' = Win32Guest undefined

--traverseDMapWithKeyWithAdjustWithMove (\k v -> unWin32Guest (f k v)) dm0 dm'

instance (RH.ReflexHost t) => R.NotReady t (Win32Guest t m) where
  {-# INLINEABLE notReadyUntil #-}
  notReadyUntil _ = pure ()

  {-# INLINEABLE notReady #-}
  notReady = pure ()

-- This is the code that runs our frp applications.
host ::
  ( forall t m.
    Win32GuestConstraints t m =>
    Win32Guest t m (R.Event t ())
  ) ->
  IO ()
host guest =
  -- we use the Spider implementation of Reflex.
  R.runSpiderHost $ mdo
    (postBuild, postBuildTriggerRef) <- RH.newEventWithTriggerRef
    triggerEventChan <- liftIO CCC.newChan
    triggerEventTChan <- liftIO CTC.newTChanIO
    mergerThread <-
      liftIO $ CC.forkIO $ forever $
        STM.atomically . CTC.writeTChan triggerEventTChan
          =<< CCC.readChan triggerEventChan

    ((eQuit, DoUndo (do_, undo)), R.FireCommand fire) <-
      R.hostPerformEventT
        . flip R.runTriggerEventT triggerEventChan
        . flip R.runPostBuildT postBuild
        . flip CMR.runReaderT (GuestEnv (eventLoop triggerEventTChan runFrameAndCheckQuit))
        . CMW.runWriterT
        . unWin32Guest
        $ guest

    hQuit <- RH.subscribeEvent eQuit

    let runFrameAndCheckQuit firings = do
          lmQuit <- fire firings $ RH.readEvent hQuit >>= sequence
          when (any DM.isJust lmQuit)
            $ liftIO
            $ WG.postQuitMessage 0

    RH.runHostFrame do_
    Ref.readRef postBuildTriggerRef
      >>= DF.traverse_ (\t -> runFrameAndCheckQuit [t ==> ()])

    -- finally, we start our loop
    liftIO $ () <$ messagePump Nothing
    RH.runHostFrame undo

    liftIO $ CC.killThread mergerThread

eventLoop ::
  CTC.TChan [DSum (RT.EventTriggerRef t) RT.TriggerInvocation] ->
  ([DSum (RH.EventTrigger t) CMI.Identity] -> RI.SpiderHost RI.Global a) ->
  IO ()
eventLoop triggerEventTChan runFrameAndCheckQuit =
  R.runSpiderHost $ do
    let loop :: RI.SpiderHost RI.Global ()
        loop = do
          isEmpty <- liftIO $ STM.atomically $ CTC.isEmptyTChan triggerEventTChan
          unless isEmpty $ do
            eventsAndTriggers <- liftIO $ STM.atomically $ CTC.readTChan triggerEventTChan
            let prepareFiring ::
                  (Ref.MonadRef m, Ref.Ref m ~ Ref.Ref IO) =>
                  DSum (R.EventTriggerRef t) R.TriggerInvocation ->
                  m (Maybe (DSum (RH.EventTrigger t) CMI.Identity))
                prepareFiring (R.EventTriggerRef er :=> R.TriggerInvocation x _) =
                  Ref.readRef er <&> fmap (==> x)

            _ <-
              DM.catMaybes <$> DT.for eventsAndTriggers prepareFiring
                >>= runFrameAndCheckQuit

            liftIO . DF.for_ eventsAndTriggers $
              \(_ :=> RT.TriggerInvocation _ cb) -> cb
            loop
    loop

messagePump :: Maybe WG.HWND -> IO ()
messagePump hwnd = WG.allocaMessage $ \msg ->
  let pump = do
        r :: Either IOError Bool <-
          CE.try $ WG.getMessage msg hwnd
        when (either (const False) id r) $ do
          () <$ WG.translateMessage msg
          () <$ WG.dispatchMessage msg
          pump
   in pump
