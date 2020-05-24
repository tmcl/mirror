{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Win32.Toolbar where

import Prelude hiding (putStrLn, print)
import Control.Applicative (empty)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.Default (def)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word (Word16)
import Foreign.Ptr (ptrToIntPtr)
import Graphics.Win32
import Graphics.Win32.Toolbar
import Reflex as R
import Reflex.Host.Class
import Reflex.Win32.Host
import Reflex.Win32.Window
import Win32.Utils
import Win32.Utils.Toolbar

data ToolbarIn t = ToolbarIn
  { dtbiBitmapSize :: R.Dynamic t (Word16, Word16),
    dtbiButtonSize :: R.Dynamic t (Word16, Word16),
    dtbiStyle :: R.Dynamic t ToolbarStyle,
    dtbiVisible :: R.Dynamic t Bool,
    dtbiResponders :: R.Dynamic t Responder
  }


data ToolbarOut t = ToolbarOut
  { dtboHwnd :: R.Dynamic t (Maybe HWND),
    dtboSize :: R.Dynamic t (Pos, Pos, Pos, Pos)
  }

isCustom :: ImageType -> Bool
isCustom Custom {} = True
isCustom _ = False

isStandardList :: StandardImageList -> ImageType -> Bool
isStandardList target (StandardImage sil _) = target == sil
isStandardList _ _ = False


type Win32Toolbar t m a = ReaderT (Win32ToolbarEnv t) m a

data Win32ToolbarEnv t = Win32ToolbarEnv
  { dtbeHwnd :: Dynamic t (Maybe HWND),
    dtbeAddButton :: ButtonInfo -> IO ButtonInfoId,
    dtbeRemoveButton :: ButtonInfoId -> HostFrame t (),
    etbeCommand :: Event t (NotificationCode, ControlIdentifier)
  }


rebuildToolbar ::
  IORef (Map Word16 ButtonInfo) ->
  MVar (Maybe HIMAGELIST) ->
  (((Word16, Word16), (Word16, Word16)), HWND) ->
  IO ()
rebuildToolbar
  buttonInfo
  mvImageList
  (((bmpWidth, bmpHeight), (btnWidth, btnHeight)), hwnd) = do
    () <$ sendMessage hwnd tB_BUTTONSTRUCTSIZE sizeTBBUTTON 0
    btnInfos <- readIORef buttonInfo
    cnt <- sendMessage hwnd tB_BUTTONCOUNT 0 0
    forM_ [1 .. cnt] $ \_ ->
      sendMessage hwnd tB_DELETEBUTTON 0 0
    _ <- sendMessage hwnd tB_SETBUTTONSIZE 0 (makelparam btnWidth btnHeight)

    let imageTypes = M.elems $ biImageType <$> btnInfos
    let imagesNeeded = imageTypes
          & fmapMaybe \case
            NoImage -> Nothing
            Separator _ -> Nothing
            StandardImage sil _ -> Just $ Just sil
            Custom _ _ -> Just Nothing
    let imageListsNeeded = nub imagesNeeded

    let imageListId = 0

    imageList <-
      imageList_Create
        (fromIntegral bmpWidth)
        (fromIntegral bmpHeight)
        (iLC_COLOR16 .|. iLC_MASK)
        (fromIntegral $ length imagesNeeded)
        0
    --todo maybe i should assert the equality of these two. hmm.
    _oldImageList <-
      sendMessage
        hwnd
        tB_SETIMAGELIST
        (fromIntegral imageListId)
        (fromIntegral $ ptrToIntPtr $ unHIMAGELIST imageList)
    oldImageList <- swapMVar mvImageList (Just imageList)
    mapM_ imageList_Destroy oldImageList

    imageLists <- M.fromList <$> forM (fmapMaybe id imageListsNeeded) \stdListId -> do
      offset <-
        sendMessage
          hwnd
          tB_LOADIMAGES
          (unStandardImageList stdListId)
          (fromIntegral (ptrToIntPtr hINST_COMMCTRL))
      return (stdListId, fromIntegral offset)

    btns <- forM (M.toList btnInfos) \(commandId, ButtonInfo {..}) -> do
      let tbCommand = fromIntegral commandId
      let tbState = biState
      let tbStyle = biStyle .|. bool 0 bTNS_SEP (isSeparator biImageType)
      let tbData = 0
      let tbString = Right biString

      tbBitmap <- runMaybeT case biImageType of
        NoImage -> empty
        Separator size -> return size
        StandardImage sil sit -> do
          offset <- MaybeT $return $ M.lookup sil imageLists
          return $ fromIntegral $ makelong (sit + offset) imageListId
        Custom hbitmap hbitmapMask -> do
          img <- liftIO $ imageList_Add imageList hbitmap hbitmapMask
          return $ fromIntegral $ makelong img imageListId

      return TBBUTTON {..}
    unless (null btns) $ do
      addButtonsRes <- sendTB_ADDBUTTONS hwnd btns
      when (addButtonsRes == 0) $ failIfFalse_ "TB_ADDBUTTONS" (return False)
    void $sendMessage hwnd tB_SETBUTTONSIZE 0 (makelparam btnWidth btnHeight)
    void $sendMessage hwnd tB_AUTOSIZE 0 0


toolbarButton ::
  (Monad m, MonadIO m, Reflex t, WriteDoUndo t m) =>
  ImageType ->
  ToolbarButtonState ->
  ToolbarButtonStyle ->
  String ->
  Win32Toolbar t m (Event t NotificationCode)
toolbarButton biImageType biState biStyle biString = do
  addButton <- asks dtbeAddButton
  removeButton <- asks dtbeRemoveButton
  btnId <- liftIO $ addButton ButtonInfo {..}
  ev <- asks etbeCommand
  tellDoUndo $ DoUndo (return (), removeButton btnId)
  return $ fmapMaybe (myNotifications btnId) ev
  where
    myNotifications btnId (notificationCode, controlId) =
      bool Nothing (Just notificationCode) (btnId == controlId)


toolbar ::
  ( Ref m ~ Ref IO,
    Ref (HostFrame t) ~ Ref IO,
    Reflex t,
    ReflexHost t,
    MonadHold t m,
    MonadIO m,
    MonadRef (HostFrame t),
    PrimMonad (HostFrame t),
    MonadIO (HostFrame t)
  ) =>
  ToolbarIn t ->
  Win32Toolbar t (Win32Guest t m) a ->
  Win32Window t m (ToolbarOut t, a)
toolbar ToolbarIn {..} buttons = do
  let dwiWindowStyleEx = pure 0
  let cwiClass = RegisteredClassName tOOLBARCLASSNAME
  let dwiWsExTopmost = pure False
  let dwiWsVisible = dtbiVisible
  let dwiHfont = Nothing
  let dwiTitle = pure ""
  let dwiWindowStyle = (.|.) (wS_CHILD .|. wS_CLIPCHILDREN) . untbStyle <$> dtbiStyle
  let wfsiStandard = CreateWindowParams {..}
  let size = def {cwiInitialPos = (Just 0, Just 0, Just 0, Just 0)}
  let wi = def {dwiResponse = dtbiResponders}

  (wo, _, ()) <- windowUserSized wfsiStandard wi size $ return ()

  dtboHwnd <- R.holdDyn Nothing (ewoHwnd wo)

  eParentMessage <- asks wcParentMessages
  let resize = eParentMessage wM_SIZE
  eResized <- performEvent $
    fmapMaybe id (dtboHwnd `tagDyn` resize) <&> \hwnd ->
      void $ liftIO $ sendMessage hwnd tB_AUTOSIZE 0 0

  (eNeedsRebuild, triggerNeedsRebuild) <- newTriggerEvent
  nextId <- liftIO $ newIORef 0
  buttonInfo <- liftIO $ newIORef mempty
  let tbEnv =
        Win32ToolbarEnv
          { dtbeAddButton = \bi -> do
              btnId <- modifyIORef nextId (+ 1) >> readIORef nextId 
              modifyIORef buttonInfo (M.insert btnId bi)
              triggerNeedsRebuild ()
              return btnId,
            dtbeRemoveButton = \btnId -> liftIO do
              modifyIORef buttonInfo (M.delete btnId)
              triggerNeedsRebuild (),
            dtbeHwnd = dtboHwnd,
            etbeCommand = ewoCommand wo
          }

  a <- withReaderT (const tbEnv) buttons

  mvImageList <- liftIO $ newMVar Nothing

  let toolbarRenderInfo = do
        buttonSize <- dtbiButtonSize
        bitmapSize <- dtbiBitmapSize
        mhwnd <- dtboHwnd
        return ((bitmapSize, buttonSize), mhwnd)
  let externalChange = fmapMaybe sequence $ updated toolbarRenderInfo
  let internalChange = fmapMaybe sequence $ current toolbarRenderInfo `tag` eNeedsRebuild

  let updateToolbar = leftmost [externalChange, internalChange]

  eRebuilt <- performEvent $ updateToolbar <&> liftIO . rebuildToolbar buttonInfo mvImageList
  let eSizeInfoChanged = leftmost [eResized, eRebuilt]
  etboSize <-
    performEvent $
      dtboHwnd `tagDyn` eSizeInfoChanged & fmapMaybe id
        <&> fmap (rectMap fromIntegral . absToRel) . liftIO . getWindowRect
  dtboSize <- holdDyn (0, 0, 0, 0) etboSize

  return (ToolbarOut {..}, a)

