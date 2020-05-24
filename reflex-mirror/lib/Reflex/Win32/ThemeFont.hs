{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Win32.ThemeFont (themeFont) where

import qualified Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Graphics.Win32 as WG
import qualified Graphics.Win32.GDI.Font as WGF
import qualified Reflex
import qualified Reflex.Host.Class as RH
import qualified Reflex.Win32.Host
import qualified System.Win32.Dpi
import qualified System.Win32.Info.Parameters as SWIP
import Prelude hiding (print, putStrLn)

themeFont ::
  forall t m.
  ( Reflex.Reflex t,
    Monad m,
    Reflex.PostBuild t m,
    MonadIO m,
    Reflex.TriggerEvent t m,
    Reflex.PerformEvent t m,
    MonadIO (Reflex.Performable m),
    Reflex.MonadHold t m,
    Monad (RH.HostFrame t),
    MonadIO (RH.HostFrame t),
    Reflex.Win32.Host.WriteDoUndo t m
    -- RH.ReflexHost t
  ) =>
  System.Win32.Dpi.DpiFunctions ->
  Reflex.Dynamic t Int ->
  m (Reflex.Dynamic t WG.HFONT)
themeFont dpiFunctions dDpi = do
  let dDpiUINT = fromIntegral <$> dDpi

  pb <- Reflex.getPostBuild
  let eInitDpi = Reflex.current dDpiUINT `Reflex.tag` pb

  let createFont dpi = do
        ncm <- System.Win32.Dpi.getNonClientMetricsForDpi dpiFunctions dpi
        WGF.createFontIndirect (SWIP.ncmMessageFont ncm)

  initDpi <- liftIO $ System.Win32.Dpi.getDpiForSystem_ dpiFunctions
  initFont <- liftIO $ createFont initDpi
  mvHfont <- liftIO $ Control.Concurrent.MVar.newMVar initFont

  let updateFont onNewFont newFont = () <$ do
        oldFont <- Control.Concurrent.MVar.swapMVar mvHfont newFont
        () <$ onNewFont newFont
        WG.deleteFont oldFont

  eHfont <-
    Reflex.performEventAsync $
      (\a b -> liftIO (createFont a >>= updateFont b))
        <$> Reflex.leftmost [eInitDpi, Reflex.updated dDpiUINT]

  let tellDoUndo :: Reflex.Win32.Host.DoUndo t -> m ()
      tellDoUndo = Reflex.Win32.Host.tellDoUndo

  tellDoUndo $
    Reflex.Win32.Host.DoUndo
      ( return (),
        liftIO $ Control.Concurrent.MVar.takeMVar mvHfont >>= WG.deleteFont
      )

  Reflex.holdDyn initFont eHfont
