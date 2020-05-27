{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}

module Reflex.Win32.Window 
  (windowUserSized
  , runWindow
  , Message(..)
  , Win32WindowConstraints
  , MaybeWindowProc
  , windowCodeSized
  , CreateWindowParams(..)
  , WindowIn(..)
  , Win32WindowEnv(..)
  , WindowSizeInUserControlled(..)
  , WindowSizeInCodeControlled(..)
  , WindowSizeOutUserControlled(..)
  , WindowSizeOutCodeControlled(..)
  , WindowOut(..)
  , WindowClassIdentifier(..)
  , click
  , tagDyn
  , Win32Window
  , Responder
  , module Win32UtilsMessage
  ) where

import Control.Concurrent.MVar
import Prelude hiding (putStrLn, print)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Default (Default, def)
import Foreign.C.Types
import Control.Lens (_3, _4, (^.))
import Control.Monad (void, join, forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT, lift)
import Control.Monad.Ref (Ref, MonadRef, newRef, writeRef, readRef)
import Control.Monad.Primitive (PrimMonad)
import Data.Bool (bool)
import qualified Data.IntMap as IntMap
import Data.Map (Map, (!?)) 
import qualified Data.Map as M
import Foreign hiding (void)
import Graphics.Win32 hiding (getParent)
import qualified Reflex 
import qualified Reflex.Network as RN
import qualified Reflex.Host.Class as RH
import System.Win32 (getModuleHandle)
import Win32.Utils (Message(..), NotificationCode, ControlIdentifier, isButtonClick, makelparam, sizeDim, rectMap, sizeFromMessage, absToRel)
import Reflex.Win32.Host 
import Graphics.Win32.Toolbar (tOOLBARCLASSNAME)
import qualified Win32.Utils as Win32UtilsMessage (Message(..))

type Win32Window t m = ReaderT (Win32WindowEnv t) (Win32Guest t m)
data Win32WindowEnv t = Win32WindowEnv 
  { wcParent :: IO (Maybe HWND)
  , wcParentMessages :: WindowMessage -> Reflex.Event t Message
  , wcAddToParent :: HWND -> ((NotificationCode, ControlIdentifier) -> IO ()) -> IO ()
  , wcRemoveFromParent :: HWND -> IO ()
  , wcAddResponderToParent :: HWND -> WindowMessage -> Ref IO (WPARAM -> IO LRESULT) -> IO ()
  , wcRemoveResponderFromParent :: HWND -> WindowMessage -> IO ()
  , wcEventLoop :: IO ()
  }

runWindow ::
  forall t m a.
  (Reflex.Win32.Host.Win32GuestConstraints t m) =>
  Reflex.Win32.Window.Win32Window t m a ->
      Reflex.Win32.Host.Win32Guest t m a
runWindow window = do
  -- at the moment we have to do some manual set up,
  -- and there's still two constraint sets/environments: the 
  -- basic, "you get a message loop" Win32GuestConstraints
  -- and the more advanced "now you can run your window"
  -- Win32WindowConstraints. mostly because i'm still 
  -- working on new features, and this kind of refactoring
  -- is usually something i leave till the second time around.
  -- (this should be that time, but right now i want to get 
  -- something released).

  -- this is actually the Reflex message loop. it is possible
  -- it could be shoved into the Win32 message loop, but I don't
  -- fully understand the difference between posted and sent 
  -- messages just yet and haven't got around to the experimentation
  -- i need to do to find that out. for now, it is called from each
  -- window proc. 
  myEventLoop <- Reflex.Win32.Host.Win32Guest $ lift $ Control.Monad.Reader.asks Reflex.Win32.Host.geEventLoop

  let defaultEnvironment = 
        Reflex.Win32.Window.Win32WindowEnv
          {  Reflex.Win32.Window.wcParentMessages = const Reflex.never,
            Reflex.Win32.Window.wcEventLoop = myEventLoop,
            -- these several are useful for child windows; since this is the toplevel window, they are nothing.
            Reflex.Win32.Window.wcParent = pure Nothing,
            Reflex.Win32.Window.wcAddToParent = \_ _ -> return (),
            Reflex.Win32.Window.wcRemoveFromParent = const $ return (),
            Reflex.Win32.Window.wcAddResponderToParent = \_ _ _ -> return (),
            Reflex.Win32.Window.wcRemoveResponderFromParent = \_ _ -> return ()
          }
  Control.Monad.Reader.runReaderT window defaultEnvironment 

data WindowClassIdentifier 
  = Button
  | ComboBox
  | TabControl
  | Edit
  | ListBox
  | MDIClient
  | RichEdit
  | RichEdit2
  | ScrollBar
  | Static
  | Toolbar
  | RegisteredClassName ClassName
  | RegisteredClassAtom ATOM

toClassName :: WindowClassIdentifier -> ClassName
toClassName Button = mkClassName "BUTTON"
toClassName ComboBox = mkClassName "COMBOBOX"
toClassName Edit = mkClassName "EDIT"
toClassName ListBox = mkClassName "LISTBOX"
toClassName MDIClient = mkClassName "MDICLIENT"
toClassName RichEdit = mkClassName "RichEdit"
toClassName RichEdit2 = mkClassName "RICHEDIT_CLASS"
toClassName ScrollBar = mkClassName "SCROLLBAR"
toClassName Static = mkClassName "STATIC"
toClassName Toolbar = tOOLBARCLASSNAME
toClassName TabControl = mkClassName "SysTabControl32"
toClassName (RegisteredClassName name) = name
toClassName (RegisteredClassAtom atom) = intPtrToPtr $ fromIntegral atom

isStandardClass :: WindowClassIdentifier -> Bool
isStandardClass Button = True
isStandardClass Toolbar = True
isStandardClass TabControl = True
isStandardClass ComboBox = True
isStandardClass Edit = True
isStandardClass ListBox = True
isStandardClass MDIClient = True
isStandardClass RichEdit = True
isStandardClass RichEdit2 = True
isStandardClass ScrollBar = True
isStandardClass Static = True
isStandardClass (RegisteredClassName _) = False
isStandardClass (RegisteredClassAtom _) = False

data CreateWindowParams t = CreateWindowParams 
   { dwiWindowStyleEx :: Reflex.Dynamic t WindowStyle -- ^ WindowStyleEx, check msdn. but dwiWsExTopmost to control that param. other updates presently ignored
   , cwiClass :: WindowClassIdentifier -- ^ window class
   , dwiTitle :: Reflex.Dynamic t String  -- ^ window title
   , dwiWindowStyle :: Reflex.Dynamic t WindowStyle -- ^ WindowStyle, check msdn. but use dwiWsVisible to control that property. updates presently ignored.
   , dwiWsExTopmost :: Reflex.Dynamic t Bool -- ^ is this window topmost? (i.e. above all non-topmost windows?). updates considered.
   , dwiWsVisible :: Reflex.Dynamic t Bool -- ^ is this window visible? updates considered.
   , dwiHfont :: Maybe (Reflex.Dynamic t HFONT) -- ^ sends a WM_SETFONT message whenever it changes/whenever the window is set. 
   }

instance Applicative (Reflex.Dynamic t) => Default (CreateWindowParams t) where
   def = CreateWindowParams 
      { dwiWindowStyleEx = pure 0
      , cwiClass = Button
      , dwiTitle = pure "" 
      , dwiWindowStyle = pure 0
      , dwiWsExTopmost = pure False
      , dwiWsVisible = pure True
      , dwiHfont = Nothing
      }

type Responder = (Map WindowMessage (HWND -> WPARAM -> LPARAM -> IO (Maybe LRESULT)))
type MaybeWindowProc = HWND -> WindowMessage -> WPARAM -> LPARAM -> IO (Maybe LRESULT)

-- | both parameters are roughly equivalent. the second one is run after child responders, so if you want
-- to respond to WM_COMMAND or something, you could do it in dwiEarlyResponder but not dwiResponse.
-- i don't recall what problem it solved and may remove it once i've inspected the rest of my code.
data WindowIn t = WindowIn 
   { dwiResponse :: Reflex.Dynamic t Responder 
   , dwiEarlyResponder :: Reflex.Dynamic t MaybeWindowProc
   }
instance Applicative (Reflex.Dynamic t) => Default (WindowIn t) where
  def = WindowIn 
    { dwiResponse = pure mempty
    , dwiEarlyResponder = pure $ \_hwnd _wmsg _wParam _lParam -> return Nothing
    }

data WindowOut t = WindowOut
   { ewoMessage :: WindowMessage -> Reflex.Event t Message -- ^ a window message received by the window procedure
   , ewoCommand :: Reflex.Event t (NotificationCode, ControlIdentifier)  -- ^ a WM_COMMAND message received by parent's window proc. it is directed here when LPARAM == this window's HWND. the parameters are the HIWORD and LOWORD of WPARAM. check msdn WM_COMMAND.
   , ewoHwnd :: Reflex.Event t (Maybe HWND) -- ^ the HWND of this window. should only be sent Just HWND once at the beginning and Nothing at the end of the lifetime of the widget. 
   }

data WindowSizeInUserControlled t = WindowSizeInUserControlled
   { cwiInitialPos :: (Maybe Pos, Maybe Pos, Maybe Pos, Maybe Pos) -- ^ initial position in terms of "Graphics.Win32.createWindowEx"
   , ewiSetPos :: Reflex.Event t (Int, Int, Int, Int) -- ^ updated position in terms of "Graphics.Win32.setWindowPos".
   }
instance Reflex.Reflex t => Default (WindowSizeInUserControlled t) where
  def = WindowSizeInUserControlled 
   { cwiInitialPos = (Nothing, Nothing, Nothing, Nothing)
   , ewiSetPos = Reflex.never
   }

data WindowSizeOutUserControlled t = WindowSizeOutUserControlled
   { dwoWindowSizeInfo :: Reflex.Dynamic t (Pos, Pos) -- ^ GetWindowRect i.e. including the non-client area (borders etc)
   , dwoClientSizeInfo :: Reflex.Dynamic t (Pos, Pos) -- ^ GetClientRect i.e. excluding the non-client area
   }

newtype WindowSizeInCodeControlled t = WindowSizeInCodeControlled
   { dwiPos :: Reflex.Dynamic t (Pos, Pos, Pos, Pos) -- ^position of the window in terms of both createWindowEx and setWindowPos.
   }

-- | contains no relevant information and i will probably remove it.
data WindowSizeOutCodeControlled t = WindowSizeOutCodeControlled 

-- | convenience function. checks button clicks
click :: Reflex.Reflex t => WindowOut t -> Reflex.Event t ()
click = (<$) () . Reflex.ffilter isButtonClick . ewoCommand 

performStandardUpdates :: ( MonadIO (Reflex.Performable m)
                          , Reflex.PerformEvent t m
                          , Reflex.Reflex t
                          , Reflex.PostBuild t m
                          )
                       => CreateWindowParams t 
                       -> HWND
                       -> m ()
performStandardUpdates CreateWindowParams {..} hwnd = do
  pb <- Reflex.getPostBuild
  let eVisible = Reflex.leftmost [Reflex.updated dwiWsVisible, Reflex.current dwiWsVisible `Reflex.tag` pb]
  Reflex.performEvent_ $ eVisible <&> \isVisible -> liftIO do
     void $ showWindow hwnd (bool sW_HIDE sW_SHOW isVisible)
  Reflex.performEvent_ $ Reflex.updated dwiTitle <&> \title -> liftIO do
     setWindowText hwnd title

  forM_ dwiHfont $ \dHfont -> do
    let eFont = Reflex.leftmost [Reflex.updated dHfont, Reflex.current dHfont `Reflex.tag` pb]
    Reflex.performEvent_ $ eFont <&> \hfont -> liftIO do
       void $ sendMessage hwnd wM_SETFONT (fromIntegral . ptrToIntPtr $ hfont)
                (makelparam 1 0)

sampleDyn :: (Reflex.MonadSample t (RH.HostFrame t), Reflex.Reflex t) => Reflex.Dynamic t a -> RH.HostFrame t a 
sampleDyn = Reflex.sample . Reflex.current

windowCodeSized :: ( Reflex.Reflex t
                , RH.ReflexHost t
                , MonadRef (Win32Guest t m)
                , Ref (Win32Guest t m) ~ Ref IO
                , Ref m ~ Ref IO
                , MonadRef (RH.HostFrame t) 
                , Ref (RH.HostFrame t) ~ Ref IO
                , MonadIO m 
                , MonadIO (RH.HostFrame t) 
                , PrimMonad (RH.HostFrame t) 
                , Reflex.MonadHold t m 
                )
             => CreateWindowParams t
             -> WindowIn t
             -> WindowSizeInCodeControlled t
             -> Win32Window t m a
             -> Win32Window t m (WindowOut t, WindowSizeOutCodeControlled t, a)
windowCodeSized createWindowParams wi WindowSizeInCodeControlled{..} _children = do
  let initialPos = Reflex.sample . Reflex.current $ rectMap Just <$> dwiPos

  (WindowOut {..}, refHwnd, child) 
     <- windowBuilder createWindowParams wi initialPos _children

  let updates = do
         postBuild <- Reflex.getPostBuild
         let newPos = Reflex.leftmost [Reflex.updated dwiPos, Reflex.current dwiPos `Reflex.tag` postBuild]
         positionWindow refHwnd newPos

  void $ RN.networkHold (return ()) (updates <$ ewoHwnd) 

  return (WindowOut {..}, WindowSizeOutCodeControlled, child)

windowUserSized :: ( Reflex.Reflex t
                , RH.ReflexHost t
                , MonadRef (Win32Guest t m)
                , Ref (Win32Guest t m) ~ Ref IO
                , Ref m ~ Ref IO
                , MonadRef (RH.HostFrame t) 
                , Ref (RH.HostFrame t) ~ Ref IO
                , MonadIO m 
                , MonadIO (RH.HostFrame t) 
                , PrimMonad (RH.HostFrame t) 
                , Reflex.MonadHold t m 
                )
             => CreateWindowParams t
             -> WindowIn t
             -> WindowSizeInUserControlled t
             -> Win32Window t m a
             -> Win32Window t m (WindowOut t, WindowSizeOutUserControlled t, a)
windowUserSized wmiStandard wi WindowSizeInUserControlled{..} _children = do
  let initialPos = pure cwiInitialPos

  (WindowOut {..}, refHwnd, child) 
     <- windowBuilder wmiStandard wi initialPos _children

  let initialSizeInfo = ( fromMaybe 0 (cwiInitialPos^._3)
                        , fromMaybe 0 (cwiInitialPos^._4))

  let eNewSizeInfo = sizeDim . sizeFromMessage <$>  ewoMessage wM_SIZE 
  eInitialSize <- Reflex.performEvent $ Reflex.fmapMaybe id ewoHwnd <&> \hwnd -> do
     (_, _, x2, y2) <- liftIO $ absToRel <$> getClientRect hwnd
     return (fromIntegral x2, fromIntegral y2)
  dwoClientSizeInfo <- Reflex.holdDyn initialSizeInfo $
     Reflex.leftmost [eInitialSize, eNewSizeInfo]

  let windowSize hwnd = do
        eSize <- Reflex.performEvent $ Reflex.updated dwoClientSizeInfo <&> \_ -> liftIO do
           (_, _, w, h) <- liftIO $ absToRel <$> getWindowRect hwnd
           return (fromIntegral w, fromIntegral h)
        Reflex.holdDyn initialSizeInfo eSize
  dwoWindowSizeInfo <- join <$> RN.networkHold (pure $ pure initialSizeInfo)
     (windowSize <$> Reflex.fmapMaybe id ewoHwnd)

  let updates = positionWindow refHwnd ewiSetPos

  void $ RN.networkHold (return ()) (updates <$ ewoHwnd) 
    -- todo: consider passing hwnd in instead of using the ref

  return (WindowOut {..}, WindowSizeOutUserControlled {..}, child)

positionWindow :: (Reflex.PerformEvent t1 m, MonadIO (Reflex.Performable m),
                          Integral a, Integral a, Integral a, Integral a) =>
                        IO (Maybe HWND) -> Reflex.Event t1 (a, a, a, a) -> m ()
positionWindow refHwnd ePos =
         Reflex.performEvent_ $ ePos <&> \(x', y', w', h') -> liftIO $ do
             mhwnd <- refHwnd
             forM_ mhwnd $ \hwnd -> do
               let (x, y, w, h) = (fromIntegral x', fromIntegral y', fromIntegral w', fromIntegral h')
               setWindowPos hwnd nullPtr x y w h 0

type Win32WindowConstraints t m = ( MonadRef (RH.HostFrame t)
                 , MonadIO (RH.HostFrame t)
                 , MonadIO m
                 , PrimMonad (RH.HostFrame t)
                 , RH.ReflexHost t
                 , Reflex.MonadHold t m
                 , Ref (RH.HostFrame t) ~ Ref IO
                 , Ref m ~ Ref (RH.HostFrame t)
                 , Ref m ~ Ref IO
                 )

windowBuilder ::  forall t m c. (Win32WindowConstraints t m)
               => CreateWindowParams t
               -> WindowIn t
               -> RH.HostFrame t (Maybe Pos, Maybe Pos, Maybe Pos, Maybe Pos)
               -> ReaderT (Win32WindowEnv t) (Win32Guest t m) c
               -> ReaderT
                            (Win32WindowEnv t)
                            (Win32Guest t m)
                            (WindowOut t, IO (Maybe HWND), c)
windowBuilder wmiStandard@CreateWindowParams{..} WindowIn{..} getInitialPos children = do
  rmHwndParent <- asks wcParent
  eventLoop <- asks wcEventLoop

  refHwnd <- liftIO newEmptyMVar
  refChildren <- newRef mempty

  addToParent <- asks wcAddToParent
  removeFromParent <- asks wcRemoveFromParent

  pb <- Reflex.getPostBuild

  refResponders <- newRef mempty
  Reflex.performEvent_ $ Reflex.leftmost [dwiResponse `tagDyn` pb, Reflex.updated dwiResponse] <&> \v -> 
    writeRef refResponders v

  refEarlyResponder <- newRef $ \_hwnd _wmsg _wparam _lparam -> return Nothing
  Reflex.performEvent_ $ Reflex.leftmost [dwiEarlyResponder `tagDyn` pb, Reflex.updated dwiEarlyResponder] <&> 
    writeRef refEarlyResponder 

  (ewoHwnd, eHwndTrigger) <- Reflex.newTriggerEvent
  (ewoCommand, ewoCommandTrigger) <- Reflex.newTriggerEvent
  (ewoMessage', eMessageTrigger) <- Reflex.newTriggerEvent
  let ewoMessage = Reflex.selectInt (Reflex.fanInt ewoMessage') . fromIntegral
  let onMessage wmsg hwnd wParam lParam = eMessageTrigger 
         (IntMap.singleton (fromIntegral wmsg) $ Message hwnd wParam lParam)

  let
    clearWindowReferences = do
       eHwndTrigger Nothing
       mHwnd <- swapMVar refHwnd Nothing
       mapM_ removeFromParent mHwnd 

  let destruct = liftIO $ mapM_ destroyWindow =<< readMVar refHwnd


  let
    construct = do
      initialPos <- getInitialPos
      windowStyleExPart <- sampleDyn dwiWindowStyleEx
      title <- sampleDyn dwiTitle
      windowStyle <- sampleDyn dwiWindowStyle
      wsExTopmost <- sampleDyn dwiWsExTopmost
      let windowStyleEx = windowStyleExPart .|. bool 0 wS_EX_TOPMOST wsExTopmost
      let (x, y, w, h) = rectMap maybePos initialPos

      hinstance <- liftIO $ getModuleHandle Nothing
      
      mHwndParent <- liftIO rmHwndParent
      hwnd <- liftIO $ withTString title $ \szTitle ->
         c_CreateWindowEx
           windowStyleEx
           (toClassName cwiClass)
           szTitle
           windowStyle
           x y w h
           (maybePtr mHwndParent) 
           nullPtr
           hinstance
           nullPtr
      liftIO $ addToParent hwnd ewoCommandTrigger

      -- for a standard class, we only intercept the WM_NCDESTROY message to clean up after ourselves. 
      -- the rest are handled via the parent.
      -- i may completely change my mind on this.
      if isStandardClass cwiClass
        then mdo
          let callOld = callWindowProc oldWndProc 
          let freeReferences = wndProcFree (freeHaskellFunPtr wndProc >> clearWindowReferences)
          (wndProc, oldWndProc) <- setTrueWindowClosure hwnd $
             wndProcBuilder [freeReferences] callOld
          return ()

        else mdo
          let callOld = callWindowProc oldWndProc 
          let handleChildren = wndProcChildren eventLoop refChildren
          let triggerEvents = wndProcStandard eventLoop onMessage refResponders 
          let freeReferences = wndProcFree (freeHaskellFunPtr wndProc >> clearWindowReferences)
          let earlyResponder hwnd_ wmsg wparam lparam = do
                responder <- readRef refEarlyResponder
                responder hwnd_ wmsg wparam lparam
          (wndProc, oldWndProc) <- setTrueWindowClosure hwnd $
             wndProcBuilder [freeReferences, earlyResponder, handleChildren, triggerEvents] callOld
          return ()
            
      let mhwnd = Just hwnd
      liftIO $ putMVar refHwnd mhwnd
      liftIO $ eHwndTrigger mhwnd

  tellDoUndo $ DoUndo (construct, destruct)

  let updates = performStandardUpdates wmiStandard 

  void $ RN.networkHold (return ()) (maybe (return ()) updates <$> ewoHwnd) 
  -- we ignore the result of RN.networkHold because it is tantamount to () <$ ewoHwnd
  --

  myEventLoop <- asks wcEventLoop
  let 
    childEnv = Win32WindowEnv
      { wcParent = readMVar refHwnd
      , wcParentMessages = ewoMessage
      , wcAddToParent = \hwnd callback -> do
          oldChildren <- readRef refChildren
          let newChildren = M.insert hwnd callback oldChildren
          writeRef refChildren newChildren
      , wcRemoveFromParent = \hwnd -> do
          oldChildren <- readRef refChildren
          let newChildren = M.delete hwnd oldChildren
          writeRef refChildren newChildren
      , wcAddResponderToParent = \_ _ _ -> return ()
      , wcRemoveResponderFromParent = \_ _ -> return ()
      , wcEventLoop = myEventLoop
      }
  
  a <- local (const childEnv) children
  
  return (WindowOut {..}, readMVar refHwnd, a)

setTrueWindowClosure :: MonadIO m => HWND -> WindowClosure -> m (FunPtr WindowClosure, FunPtr WindowClosure)
setTrueWindowClosure hwnd windowProc = liftIO $ do
  newWindowProc <- mkWindowClosure windowProc 
  oldWindowClosure <- c_SetWindowLongPtr hwnd gWLP_WNDPROC (castFunPtrToPtr newWindowProc)
  return (newWindowProc, castPtrToFunPtr oldWindowClosure)

type EventLoop = IO ()

wndProcBuilder :: [MaybeWindowProc] -> WindowClosure -> WindowClosure
wndProcBuilder [] defProc hwnd wmsg wparam lparam = defProc hwnd wmsg wparam lparam
wndProcBuilder (proc:procs) defProc hwnd wmsg wparam lparam = do
  res <- proc hwnd wmsg wparam lparam
  case res of
    Nothing -> wndProcBuilder procs defProc hwnd wmsg wparam lparam
    Just lresult -> return lresult

wndProcFree
  :: IO ()
  -> MaybeWindowProc
wndProcFree freeReferences _ wmsg _ _ 
  | wmsg == wM_NCDESTROY = Just 0 <$ freeReferences
  | otherwise = return Nothing

wndProcChildren
  :: EventLoop
  -> Ref IO (Map HWND ((NotificationCode, ControlIdentifier) -> IO ()))
  -> MaybeWindowProc
wndProcChildren eventLoop refChildren _hwnd wmsg wParam lParam
  | wmsg == wM_COMMAND = Just 0 <$ do
      let code = hIWORD $ fromIntegral wParam
      let identifier = lOWORD $ fromIntegral wParam
      let forHwnd = intPtrToPtr $ fromIntegral lParam
      mapChildren <- readRef refChildren
      let mChildTrigger = mapChildren !? forHwnd
      forM_ mChildTrigger ($ (code, identifier)) 
      eventLoop
  | otherwise = pure Nothing

wndProcStandard 
  :: EventLoop
  -> (WindowMessage -> HWND -> WPARAM -> LPARAM -> IO ())
  -> Ref IO (Map WindowMessage (HWND -> WPARAM -> LPARAM -> IO (Maybe LRESULT)))
  -> MaybeWindowProc
wndProcStandard eventLoop trigger refResponders hwnd wmsg wParam lParam
  | wmsg == 20 = return Nothing 
  | wmsg == 127 = return Nothing -- WM_SETICON - i haven't worked out why this is eternally sent
  |otherwise = do
    trigger wmsg hwnd wParam lParam >> eventLoop 
    responders <- readRef refResponders
    let responder = responders !? wmsg
    join <$> forM responder (\r -> r hwnd wParam lParam <* eventLoop)
      
tagDyn :: Reflex.Reflex t => Reflex.Dynamic t a -> Reflex.Event t b -> Reflex.Event t a
tagDyn = Reflex.tag . Reflex.current
  
foreign import ccall "CallWindowProcW" callWindowProc
  :: FunPtr WindowClosure -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
