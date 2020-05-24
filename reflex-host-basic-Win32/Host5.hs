{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import qualified Control.Monad as CM
import qualified Control.Monad.Fix as CMF
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Ref as CMR
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (readIORef, IORef)
import qualified Control.Monad.Trans.Control as CMTC
import qualified Control.Monad.Reader as CMR
import qualified Control.Monad.Primitive as CMP
import qualified Control.Exception as CE

import Data.Dependent.Sum (DSum((:=>)))
import qualified Data.List as DL
import qualified Data.Maybe as DM

import qualified Reflex as R
import qualified Reflex.Spider.Internal as RI
import qualified Reflex.Host.Class as RH

import qualified System.Win32 as WS
import qualified Graphics.Win32 as WG

import qualified Foreign.Ptr as FP
import qualified Foreign.Marshal.Array as FMA

-- I'm going to assume you've read through Host2.hs prior to this.
--
-- Now it's time to begin incorporating some more advanced features of 
-- the reflex stack - 'PostBuildT'. 'ieOpen' was effectively 'PostBuildT'

-- Our outputs remain the same sans comments
data Output t = Output
  { oeWrite :: R.Event t String 
  , oeQuit :: R.Event t () 
  }

-- Our new application type connects these together.
type SampleApp4 t m = 
  ( CMR.Ref m ~ CMR.Ref IO
  , RH.ReflexHost t
  , MonadIO (RH.HostFrame t)
  , CMP.PrimMonad (RH.HostFrame t)
  ) => R.Event t String
    -> R.PostBuildT t (R.PerformEventT t m) (R.Event t ())

showMessage msg = CMR.void $ liftIO $ WG.messageBox Nothing msg "saith reflex" 0

-- these leads to our first sample application that isn't embarrassingly boring.
guest :: SampleApp4 t m
guest eRead = do
  eOpen <- R.getPostBuild
  eCatOut <- R.performEvent $ liftIO (readFile "README.md") <$ eCat
  let 
    eWrite = R.leftmost 
      [ "Hi"      <$  eOpen
      , ("> " ++) <$> eMessage
      ,               eCatOut 
      , "Bye"     <$  eQuit
      ] 
       
  R.performEvent_ $ showMessage <$> eWrite
 
  pure eQuit
  where 
    eMessage =       R.ffilter (not . DL.isPrefixOf "/") eRead
    -- this command will print the README.txt file in the same directory
    -- that this program is run from
    eCat     = () <$ R.ffilter (== "/cat") eRead
    eQuit    = () <$ R.ffilter (== "/quit") eRead


-- This is the code that runs our frp applications.
host :: (forall t m. SampleApp4 t m)
     -> WG.ClassName
     -> WG.HINSTANCE
     -> IO ()
host myGuest windowClass hinstance =
  -- we use the Spider implementation of Reflex.
  R.runSpiderHost $ mdo
     (eOpen, refEOpenTrigger) <- RH.newEventWithTriggerRef
     (eRead, refEReadTrigger) <- RH.newEventWithTriggerRef

     (eQuit, R.FireCommand fire)
        <- R.hostPerformEventT $ R.runPostBuildT (myGuest eRead) eOpen

     hQuit <- RH.subscribeEvent eQuit 

     -- Our set up.
     hwnd <- liftIO $ WG.createWindowEx 0 
          windowClass
          "A mirror"
          (WG.wS_OVERLAPPEDWINDOW + WG.wS_VISIBLE)
          Nothing Nothing
          (Just 500) (Just 500)
          Nothing Nothing
          hinstance
          (wndProcMain (eventLoop fire hQuit Nothing refEReadTrigger) hwndEdit hwndButton)
     --
     -- but we also want a button that will make us quit
     hwndButton <- liftIO $ WG.createButton "Send" (WG.wS_CHILD + WG.wS_VISIBLE) WG.bS_PUSHBUTTON
       (Just 45) (Just 40) (Just 75) (Just 20)
       (Just hwnd) Nothing hinstance
     
     -- then, we need to create a harmless button to click:
     hwndEdit <- liftIO $ WG.createEditWindow "..type something.." (WG.wS_CHILD + WG.wS_VISIBLE) 0
       (Just 20) (Just 20) (Just 100) (Just 20)
       hwnd Nothing hinstance
 

     liftIO $ eventLoop fire hQuit (Just hwnd) refEOpenTrigger ()
 
     -- finally, we start our loop
     liftIO $ do
        ()<$ WG.updateWindow hwnd
        ()<$ messagePump Nothing

bN_CLICKED = 0

wndProcMain :: (String -> IO ()) -> WG.HWND -> WG.HWND -> WG.WindowClosure
wndProcMain act hwndEdit hwndButton hwnd wmsg wparam lparam 
  | wmsg == WG.wM_CREATE = 0 <$ print "hello everyone"
  | wmsg == WG.wM_CLOSE = 0 <$ WG.postQuitMessage 0
  | wmsg == WG.wM_COMMAND = 0 <$ do
      print ("wM_cOMMAND", WG.hIWORD $ fromIntegral wparam, WG.lOWORD $ fromIntegral wparam, lparam)
      let lparamHwnd = FP.intPtrToPtr $ fromIntegral lparam
      let commandCode =  WG.hIWORD $ fromIntegral wparam
      CM.when (commandCode == bN_CLICKED && lparamHwnd == hwndButton) $ do
         len <- getWindowTextLength hwndEdit
         txt <- getWindowText hwndEdit (len+1)
         act txt
  | otherwise = do
      print (wmsg, wparam, lparam)
      WG.defWindowProc (Just hwnd) wmsg wparam lparam

messagePump :: Maybe WG.HWND -> IO ()
messagePump hwnd = WG.allocaMessage $ \msg ->
  let pump = do
       r :: Either IOError Bool
         <- CE.try $ WG.getMessage msg hwnd
       CMR.when (either (const False) id r) $ do
          () <$ WG.translateMessage msg
          () <$ WG.dispatchMessage msg
          pump
  in pump

eventLoop :: ([DSum (RI.RootTrigger RI.Global) CMI.Identity]
                -> RI.ReadPhase RI.Global (Maybe ())
                -> RI.SpiderHost RI.Global [Maybe ()]) 
          -> RH.EventHandle (R.SpiderTimeline R.Global) ()
          -> Maybe WG.HWND
          -> IORef (Maybe (RH.EventTrigger (R.SpiderTimeline R.Global) v))
          -> v
          -> IO ()
eventLoop fire hQuit mHwnd eTriggerRef input = 
  R.runSpiderHost $ do
    let 
      readPhase = RH.readEvent hQuit >>= sequence

      -- We have a piece of code that responds to our output events:
      handleOutputs lmQuit = do
        liftIO . putStrLn $ "lmQuit: " ++ show lmQuit

        -- Convert the occurrence of the quit event into a 'Bool':
        return $ any DM.isJust lmQuit
      
      -- We have a piece of code to fire an event and deal with the
      -- response from the output events.
      fireAndProcess t v = do
        mETrigger <- liftIO $ readIORef t
        lmQuit <- case mETrigger of
          Nothing ->
            return []
          Just eTrigger ->
            fire [eTrigger :=> CMI.Identity v] readPhase
        -- This will return a 'Bool' indicating whether the quit event
        -- has fired.
        handleOutputs lmQuit

      -- We have a piece of code that uses that to guard some kind
      -- of continuation:
      fireProcessAndPost t v = do
        quit <- fireAndProcess t v
        CMR.when quit $ liftIO $ WG.postQuitMessage 0
          
    liftIO $ putStrLn "hello"
    fireProcessAndPost eTriggerRef input

  
          
package :: IO a -> IO a
package it = it

type Poo a = CMR.ReaderT Int IO a

reddit :: Poo ()
reddit = do
  v <- CMR.ask 
  liftIO $ print v
  liftIO $ package $ print "hello"

redditInside :: Poo ()
redditInside = liftedPackage reddit

liftedPackage :: forall m u b1 b2.
               ( Monad (u m)
               , CMR.MonadIO m
               , CMTC.MonadTransControl u
               , CMTC.StT u b1 ~ CMTC.StT u b2
               ) 
              => u IO b2 
              -> u m b1
liftedPackage action = CMTC.liftWith (\run -> liftIO $ package (run action)) >>= CMTC.restoreT . return
        
main :: IO ()
main = do
   let winClass = WG.mkClassName "standard-window-2"
   icon <- WG.loadIcon Nothing WG.iDI_APPLICATION
   cursor <- WG.loadCursor Nothing WG.iDC_ARROW
   bgBrush <- WG.createSolidBrush (WG.rgb 20 80 180)
   mainInstance <- WS.getModuleHandle Nothing
   () <$ WG.registerClass
     ( WG.cS_VREDRAW + WG.cS_HREDRAW
     , mainInstance
     , Just icon
     , Just cursor
     , Just bgBrush
     , Nothing
     , winClass
     ) 

   host guest winClass mainInstance



----------------------------------------------------------------
-- Getting window text/label
----------------------------------------------------------------
-- For getting the title bar text. 
-- If the window has no title bar or text, if the title bar is empty,
-- or if the window or control handle is invalid, the return value is zero.
-- If invalid handle throws exception.
-- If size <= 0 throws exception.

getWindowText :: WG.HWND -> Int -> IO String
getWindowText wnd size
  | size <= 0 = WG.errorWin "GetWindowTextW"
  | otherwise  = do
      FMA.allocaArray size $ \ p_buf -> do
        _ <- c_GetWindowText wnd p_buf size
        WG.failUnlessSuccess "GetWindowTextW" WS.getLastError
        WG.peekTString p_buf
foreign import ccall "windows.h GetWindowTextW"
  c_GetWindowText :: WG.HWND -> WG.LPTSTR -> Int -> IO Int

----------------------------------------------------------------
-- Getting window text length
----------------------------------------------------------------
-- For getting the title bar text length. 
-- If the window has no text, the return value is zero.
-- If invalid handle throws exception.

getWindowTextLength :: WG.HWND -> IO Int
getWindowTextLength wnd = do
  size' <- c_GetWindowTextLength wnd
  WS.failUnlessSuccess "GetWindowTextLengthW" WS.getLastError
  return size'
foreign import ccall "windows.h GetWindowTextLengthW"
  c_GetWindowTextLength :: WG.HWND -> IO Int
