{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonadComprehensions #-}

module Reflex.Win32.Dpi (dpiControl, DpiOut (..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)
import Data.Word (Word16)
import qualified Data.Map
import qualified Foreign.Ptr
import qualified Graphics.Win32
import qualified Reflex
import qualified Reflex.Win32.Window
import qualified System.Win32.Dpi
import Prelude hiding (print, putStrLn)

data DpiOut t = DpiOut
  { dDpi :: Reflex.Dynamic t Int,
    dpiResponder :: Reflex.Win32.Window.Responder,

    --win32 uses so many types at different points
    --that this is really convenient
    dNominalPixels :: Reflex.Dynamic t (Int -> Int),
    dNominalPixelsInt32 :: Reflex.Dynamic t (Int -> Int32),
    dNominalPixelsWord16 :: Reflex.Dynamic t (Int -> Word16)
  }

dpiControl ::
  ( MonadIO m,
    Reflex.MonadHold t m,
    Reflex.PerformEvent t m,
    Reflex.TriggerEvent t m,
    MonadIO (Reflex.Performable m)
  ) =>
  System.Win32.Dpi.DpiFunctions ->
  Reflex.Win32.Window.WindowOut t ->
  m (DpiOut t)
dpiControl dpiFunctions windowOut = do
  eDpiNewWindow <- Reflex.performEvent $ getDpiForWindow <$> eNewWindow
  (eDpiMessage, triggerDpiMessage) <- Reflex.newTriggerEvent
  initDpi <- liftIO (System.Win32.Dpi.getDpiForSystem_ dpiFunctions)
  dDpi <-
    Reflex.holdDyn (fromIntegral initDpi) $
      Reflex.leftmost
        [ fromIntegral <$> eDpiNewWindow,
          fromIntegral <$> eDpiMessage
        ]
  let dpiResponder =
        Data.Map.singleton
          Graphics.Win32.wM_DPICHANGED
          (respondDpiChanged triggerDpiMessage)
  let dNominalPixels = [\npx -> npx * dpi `div` 96 | dpi <- dDpi]
  let dNominalPixelsInt32 = [fromIntegral . f | f <- dNominalPixels]
  let dNominalPixelsWord16 = [fromIntegral . f | f <- dNominalPixels]
  return DpiOut {..}
  where
    getDpiForWindow = liftIO . System.Win32.Dpi.getDpiForWindow_ dpiFunctions
    eNewWindow = Reflex.fmapMaybe id (Reflex.Win32.Window.ewoHwnd windowOut)

-- we resize the window in the responder because the user experience is smoother if the resize
-- finishes before the WM_DPICHANGED message returns.
respondDpiChanged ::
  (Graphics.Win32.WORD -> IO ()) ->
  Graphics.Win32.HWND ->
  Graphics.Win32.WPARAM ->
  Graphics.Win32.LPARAM ->
  IO (Maybe Graphics.Win32.LRESULT)
respondDpiChanged triggerDpiMessage hwnd wParam lParam = Just 0 <$ do
  let dpi = Graphics.Win32.hIWORD $ fromIntegral wParam
  triggerDpiMessage dpi
  (left, top, right, bottom) <- Graphics.Win32.peekRECT (Foreign.Ptr.intPtrToPtr $ fromIntegral lParam)
  Graphics.Win32.setWindowPos
    hwnd
    Foreign.Ptr.nullPtr
    left
    top
    (right - left)
    (bottom - top)
    (Graphics.Win32.sWP_NOZORDER + Graphics.Win32.sWP_NOACTIVATE)
