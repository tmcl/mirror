{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}

module Win32.Utils.Toolbar
  ( ImageType (..),
    ButtonInfo (..),
    ButtonInfoId,
    TBBUTTON (..),
    isSeparator,
    tbClick,
    isCustom,
    isStandardList,
  )
where

import Foreign hiding (void)
import Graphics.Win32
import Graphics.Win32.Toolbar
import Reflex
import Win32.Utils (NotificationCode)

tbClick :: Reflex t => Event t NotificationCode -> Event t ()
tbClick = (<$) () . ffilter (== bN_CLICKED)

type ButtonInfoId = Word16

data ImageType
  = NoImage
  | StandardImage StandardImageList StandardImageType
  | Custom HBITMAP (Maybe HBITMAP)
  | Separator INT
  deriving (Show)

isSeparator :: ImageType -> Bool
isSeparator Separator {} = True
isSeparator _ = False

isCustom :: ImageType -> Bool
isCustom Custom {} = True
isCustom _ = False

isStandardList :: StandardImageList -> ImageType -> Bool
isStandardList target (StandardImage sil _) = target == sil
isStandardList _ _ = False

data ButtonInfo = ButtonInfo
  { biImageType :: ImageType,
    biState :: ToolbarButtonState,
    biStyle :: ToolbarButtonStyle,
    biString :: String
  }
