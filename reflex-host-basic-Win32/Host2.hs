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

-- I'm going to assume you've read through Host1.hs prior to this.
--
-- We're going to update the type of our applications.
--
-- Previously, we had a Behavior t Int as an output, but now we have
-- an 'Event t ()' as an output.
--
-- In this case, we're going to use that event to signal when
-- the application wants to stop, so that we can exit reactively.
-- (this example is probably a little pointless, since our
-- loop will stop in its own due course.)

type SampleApp2 t m = 
  ( R.Reflex t
  , R.MonadHold t m
  , CMF.MonadFix m
  , CMR.Ref m ~ CMR.Ref IO
  ) => R.Event t WG.WPARAM
    -> m (R.Event t ())

-- This is our sample frp application.
--
-- Every time our input 'Event t WPARAM' fires,
-- we will check its lOWORD. if it is 1, we quit.
guest :: SampleApp2 t m
guest e = pure $ () <$ R.ffilter ((== 1) . WG.lOWORD . fromIntegral) e

-- This is the code that runs our frp applications.
host :: (forall t m. SampleApp2 t m)
     -> WG.ClassName
     -> WG.HINSTANCE
     -> IO ()
host myGuest windowClass hinstance =
  -- we use the Spider implementation of Reflex.
  R.runSpiderHost $ do


     -- we create a new event and a trigger for it.
     (e, refETrigger) <- RH.newEventWithTriggerRef

     -- Now we set up our basic event network for use with 
     -- 'myGuest e'.
     eQuit <- RH.runHostFrame $ myGuest e

     -- This will give us an 'Event t ()' which signals 
     -- the intent to quit.
     
     -- We want to be able to work out when that event has
     -- fired, so we subscribe to the event.
     hQuit :: RH.EventHandle (R.SpiderTimeline R.Global) () <- RH.subscribeEvent eQuit 
    
     -- Our set up.
     hwnd <- liftIO $ WG.createWindowEx 0 
          windowClass
          "A mirror"
          (WG.wS_OVERLAPPEDWINDOW + WG.wS_VISIBLE)
          Nothing Nothing
          (Just 500) (Just 500)
          Nothing Nothing
          hinstance
          (wndProcMain (eventLoop refETrigger hQuit))
     
     -- then, we need to create a harmless button to click:
     _ <- liftIO $ WG.createButton "Click me!" (WG.wS_CHILD + WG.wS_VISIBLE) WG.bS_PUSHBUTTON
       (Just 20) (Just 20) (Just 75) (Just 20)
       (Just hwnd) Nothing hinstance
 
     -- but we also want a button that will make us quit
     _ <- liftIO $ WG.createButton "E&xit" (WG.wS_CHILD + WG.wS_VISIBLE) WG.bS_PUSHBUTTON
       (Just 100) (Just 20) (Just 75) (Just 20)
       (Just hwnd) (Just $ FP.intPtrToPtr 1) hinstance
 
     -- finally, we start our loop
     liftIO $ do
        ()<$ WG.updateWindow hwnd
        ()<$ messagePump Nothing

wndProcMain :: (WG.WPARAM -> IO ()) -> WG.WindowClosure
wndProcMain act hwnd wmsg wparam lparam 
  | wmsg == WG.wM_CREATE = 0 <$ print "hello everyone"
  | wmsg == WG.wM_CLOSE = 0 <$ WG.postQuitMessage 0
  | wmsg == WG.wM_COMMAND = 0 <$ act wparam
  | otherwise = WG.defWindowProc (Just hwnd) wmsg wparam lparam

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

eventLoop :: IORef (Maybe (RH.EventTrigger (R.SpiderTimeline R.Global) WG.WPARAM))
          -> RH.EventHandle (R.SpiderTimeline R.Global) ()
          -> WG.WPARAM
          -> IO ()
eventLoop refETrigger hQuit input = 
     R.runSpiderHost $ do

        meTrigger <- liftIO $ readIORef refETrigger
        mQuit <- case meTrigger of
          Nothing -> return Nothing
          Just eTrigger -> do
            RH.fireEventsAndRead [eTrigger :=> CMI.Identity input] $ do
               -- we now have a read phase that happens after th events have been fired.
               -- the main thing that we do in the 'ReadPhase' is call 'readEvent' and
               -- deal with its output
               --
               -- The event may not be ocurring, so there's a 'Maybe' in there.
               mValue <- RH.readEvent hQuit
               sequence mValue
        
        -- Again, there is a helper functions that reads the trigger
        -- reference, fires the trigger if it is not 'Nothing', and then
        -- reads an output event from a particular event handle.
        --
        -- The above block could be replaced with:
        -- mQuit <- fireEventRefAndRead eTriggerRef input hQuit

        -- The result of this block is
        --   mQuit :: Maybe ()
        -- which has filtered up through a few layers to get to us, but is still
        -- perfectly serviceable.

        -- We print out the value for debugging purposes:
        liftIO $ putStrLn $ "Output Event: " ++ show mQuit
        -- and then use it to determine if we'll continue with the loop:
        CMR.when (DM.isJust mQuit) $ liftIO $ WG.postQuitMessage 0

  
          
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
