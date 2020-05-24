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
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, IORef)
import qualified Control.Monad.Trans.Control as CMTC
import qualified Control.Monad.Reader as CMR
import qualified Control.Exception as CE

import Data.Dependent.Sum (DSum((:=>)))
import qualified Data.Maybe as DM

import qualified Reflex as R
import qualified Reflex.Host.Class as RH

import qualified System.Win32 as WS
import qualified Graphics.Win32 as WG

import qualified Foreign.Ptr as FP
import qualified Foreign.Marshal.Array as FMA

-- I'm going to assume you've read through Host2.hs prior to this.
--
-- We're going to introduce a more complex interface to our application
-- in this example.
--
-- We have our input events: 

data Input t = Input 
  { ieOpen :: R.Event t () -- ^ where ieOpen fires when the application starts
  , ieRead :: R.Event t String -- ^ and ieRead fires whenever the user enters a line of text
  }

-- we also have output events:
data Output t = Output
  { oeWrite :: R.Event t String -- ^ where oeWrite is fired to signal that we should 
                              -- print a line of text to the screen
  , oeQuit :: R.Event t () -- ^ and ieQuit is fired to signal that we should exit the app
  }

-- Our new application type connects these together.
type SampleApp3 t m = 
  ( R.Reflex t
  , R.MonadHold t m
  , CMF.MonadFix m
  , CMR.Ref m ~ CMR.Ref IO
  ) => Input t
    -> m (Output t)

-- these leads to our first sample application that isn't embarrassingly boring.
guest :: SampleApp3 t m
guest (Input eOpen eRead) = pure $ Output eWrite eQuit
  where 
    -- if the user types something other than "/quit", we interpret that as a message.
    eMessage =       R.ffilter (/= "/quit") eRead
    -- but if the user types "/quit", that means it's type to exit (sic)
    eQuit    = () <$ R.ffilter (== "/quit") eRead

    -- we'll be polite (?) and issue greeting and parting messages to the user.
    -- other than that, we'll just be echoing their input until they quit.
    -- perhaps it's 99% boring, but it's progress.
    eWrite = R.leftmost 
      [ "Hi" <$ eOpen
      , ("> " ++) <$> eMessage
      , "Bye" <$ eQuit
      ] 
   

-- This is the code that runs our frp applications.
host :: (forall t m. SampleApp3 t m)
     -> WG.ClassName
     -> WG.HINSTANCE
     -> IO ()
host myGuest windowClass hinstance =
  -- we use the Spider implementation of Reflex.
  R.runSpiderHost $ mdo
     liftIO $ putStrLn "start"
     (eOpen, refEOpenTrigger) <- RH.newEventWithTriggerRef
     (eRead, refEReadTrigger) <- RH.newEventWithTriggerRef

     liftIO $ putStrLn "running host frame"
     Output eWrite eQuit <- RH.runHostFrame $ myGuest $ Input eOpen eRead
     liftIO $ putStrLn "run host frame"

     -- This will give us an 'Event t ()' which signals 
     -- the intent to quit.
     
     -- We want to be able to work out when that event has
     -- fired, so we subscribe to the event.
     hWrite <- RH.subscribeEvent eWrite 
     liftIO $ putStrLn "ssubscribing quit"
     hQuit <- RH.subscribeEvent eQuit 
     liftIO $ putStrLn "ssubscribed quit"

     -- Our set up.
     hwnd <- liftIO $ WG.createWindowEx 0 
          windowClass
          "A mirror"
          (WG.wS_OVERLAPPEDWINDOW + WG.wS_VISIBLE)
          Nothing Nothing
          (Just 500) (Just 500)
          Nothing Nothing
          hinstance
          (wndProcMain (eventLoop hWrite hQuit Nothing refEReadTrigger) hwndEdit hwndButton)
     liftIO $ putStrLn "window all good"
     --
     -- but we also want a button that will make us quit
     hwndButton <- liftIO $ WG.createButton "Send" (WG.wS_CHILD + WG.wS_VISIBLE) WG.bS_PUSHBUTTON
       (Just 45) (Just 40) (Just 75) (Just 20)
       (Just hwnd) Nothing hinstance
     liftIO $ putStrLn "button window all good"
     
     -- then, we need to create a harmless button to click:
     hwndEdit <- liftIO $ WG.createEditWindow "..type something.." (WG.wS_CHILD + WG.wS_VISIBLE) 0
       (Just 20) (Just 20) (Just 100) (Just 20)
       hwnd Nothing hinstance
 
     liftIO $ putStrLn "edit window all good"

     liftIO $ putStrLn "running one event loop"
     liftIO $ eventLoop hWrite hQuit (Just hwnd) refEOpenTrigger ()
     liftIO $ putStrLn "ran one event loop"
 
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

eventLoop :: RH.EventHandle (R.SpiderTimeline R.Global) String
          -> RH.EventHandle (R.SpiderTimeline R.Global) ()
          -> Maybe WG.HWND
          -> IORef (Maybe (RH.EventTrigger (R.SpiderTimeline R.Global) v))
          -> v
          -> IO ()
eventLoop hWrite hQuit mHwnd eTriggerRef input = 
  R.runSpiderHost $ do
    let 
      readPhase = do
          mWrite <- RH.readEvent hWrite >>= sequence
          mQuit <- RH.readEvent hQuit >>= sequence
          return (mWrite, mQuit)

      -- We have a piece of code that responds to our output events:
      handleOutputs mWrite mQuit = do
        CMR.forM_ mWrite $ \w ->
          liftIO $ WG.messageBox mHwnd w "saith reflex" 0

        -- Convert the occurrence of the quit event into a 'Bool':
        return $ DM.isJust mQuit
      
      -- We have a piece of code to fire an event and deal with the
      -- response from the output events.
      fireAndProcess t v = do
        mETrigger <- liftIO $ readIORef t
        (mWrite, mQuit) <- case mETrigger of
          Nothing ->
            return (Nothing, Nothing)
          Just eTrigger ->
            RH.fireEventsAndRead [eTrigger :=> CMI.Identity v] readPhase
        -- This will return a 'Bool' indicating whether the quit event
        -- has fired.
        handleOutputs mWrite mQuit

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
