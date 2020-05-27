{-# LANGUAGE RecordWildCards #-}

module Win32.Utils
  ( Message (..),
    NotificationCode,
    ControlIdentifier,
    isButtonClick,
    rectMap,
    makelparam,
    makelong,
    sizeDim,
    sizeFromMessage,
    sizeTypeFromSW,
    absToRel,
    relToAbs,
  )
where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word16, Word32)
import qualified Graphics.Win32

data SizeType = SizeMaximized | SizeMinimized | SizeRestored
  deriving (Show, Eq)

sizeTypeFromWparam :: Graphics.Win32.WPARAM -> Maybe SizeType
sizeTypeFromWparam wparam
  | wparam == Graphics.Win32.sIZE_MAXIMIZED = Just SizeMaximized
  | wparam == Graphics.Win32.sIZE_MINIMIZED = Just SizeMinimized
  | wparam == Graphics.Win32.sIZE_RESTORED = Just SizeRestored
  | otherwise = Nothing

sizeTypeFromSW :: Graphics.Win32.UINT -> SizeType
sizeTypeFromSW uint
  | uint == Graphics.Win32.sW_SHOWMAXIMIZED = SizeMaximized
  | uint == Graphics.Win32.sW_SHOWMINIMIZED = SizeMinimized
  | otherwise = SizeRestored

data SizeVisibility = SizeMaxHide | SizeMaxShow
  deriving (Show, Eq)

sizeVisibilityFromWparam :: Graphics.Win32.WPARAM -> Maybe SizeVisibility
sizeVisibilityFromWparam wparam
  | wparam == Graphics.Win32.sIZE_MAXSHOW = Just SizeMaxShow
  | wparam == Graphics.Win32.sIZE_MAXHIDE = Just SizeMaxHide
  | otherwise = Nothing

data SizeEvent = SizeEvent
  { sizeType :: Maybe SizeType,
    sizeVisibility :: Maybe SizeVisibility,
    sizeDim :: (Int, Int)
  }
  deriving (Show, Eq)

sizeFromMessage :: Message -> SizeEvent
sizeFromMessage Message {..} = SizeEvent {..}
  where
    sizeType = sizeTypeFromWparam wmWparam
    sizeVisibility = sizeVisibilityFromWparam wmWparam
    sizeDim =
      ( fromIntegral $ Graphics.Win32.lOWORD $ fromIntegral wmLparam,
        fromIntegral $ Graphics.Win32.hIWORD $ fromIntegral wmLparam
      )

isButtonClick :: (Word16, Word16) -> Bool
isButtonClick = (== Graphics.Win32.bN_CLICKED) . fst

type NotificationCode = Word16

type ControlIdentifier = Word16

makelong :: Word16 -> Word16 -> Word32
makelong lo hi = lo' .|. hi'Shifted
  where
    lo' = fromIntegral lo :: Word32
    hi' = fromIntegral hi :: Word32
    hi'Shifted = hi' `shiftL` 16

makelparam :: Word16 -> Word16 -> Graphics.Win32.LPARAM
makelparam lo hi = fromIntegral $ makelong lo hi

relToAbs :: Graphics.Win32.RECT -> Graphics.Win32.RECT
relToAbs (x, y, w, h) = (x, y, w + x, h + y)

absToRel :: Graphics.Win32.RECT -> Graphics.Win32.RECT
absToRel (x1, y1, x2, y2) = (x1, y1, x2 - x1, y2 - y1)

rectMap :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
rectMap f (a, b, c, d) = (f a, f b, f c, f d)

data Message = Message
  { wmHwnd :: Graphics.Win32.HWND,
    wmWparam :: Graphics.Win32.WPARAM,
    wmLparam :: Graphics.Win32.LPARAM
  }
  deriving (Show, Eq)
