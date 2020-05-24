{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App1(main) where

import qualified Control.Monad as CM
import qualified Control.Monad.Fix as CMF
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Ref as CMR
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, IORef)
import qualified Control.Monad.Reader as CMR
import qualified Control.Exception as CE

import Data.Dependent.Sum (DSum((:=>)))

import qualified Reflex as R
import qualified Reflex.Host.Class as RH

import qualified System.Win32 as WS
import qualified Graphics.Win32 as WG

-- First, we define a type for our applications
--
-- In this case, our applications will take an
-- 'Event t String' as input and return a
-- 'Behavior t Int' as output.
--
-- While we're at it, we capture various 
-- typeclass constraints that we know we're
-- going to need in this type synonim.

type SampleApp1 t m = 
  ( R.Reflex t
  , R.MonadHold t m
  , CMF.MonadFix m
  , CMR.Ref m ~ CMR.Ref IO
  ) => R.Event t WG.WPARAM
    -> m (R.Behavior t Int)

-- This is our sample frp application.
--
-- It doesn't care what kind of event it gets
--  as an input, because we're just using it to 
--  count the events that are occurring.
guest :: SampleApp1 t m
guest e = do
  -- increment each time the event fires
  d <- R.foldDyn (+) 0 (1 <$ e)
  -- return the running count as a behavior
  return $ R.current d

-- This is the code that runs our frp applications.
host :: (forall t m. SampleApp1 t m)
     -> WG.ClassName
     -> WG.HINSTANCE
     -> IO ()
host myGuest windowClass hinstance =
  -- we use the Spider implementation of Reflex.
  R.runSpiderHost $ do


     -- we create a new event and a trigger for it.
     ( e :: R.Event t a
      , refETrigger :: CMR.Ref IO (Maybe (RH.EventTrigger t a))
      ) <- RH.newEventWithTriggerRef
     -- This guives us an event - which we need so that
     -- we can provide an input to 'myGuest' - and an event
     -- trigger.
     --
     -- 'Ref' is an abstraction over things like 'IORef' etc
     --
     -- If the event isn't being used - or if it stops
     -- being used due to changes in the network - the 
     -- 'Ref' will hold 'Nothing'.
     --
     -- If something is interested in the event, then the
     -- 'Ref' will hold 'Just trigger' where 'trigger' is 
     -- a trigger for the event
     --
     -- Now we set up our basic event network for use with 
     -- 'myGuest e'.
     b <- RH.runHostFrame $ myGuest e

     -- This will give us a 'Behavior Int' which we'll 
     -- use a little later.
     --
     -- ATthis point the event network is set up, but 
     -- there are no events firing and so nothing much is
     -- happening.
     -- 
     -- We address that by putting together an event loop
     -- to handle the firing of the event we're interested 
     -- in.
     --
     -- In this case, we're just going to acknowledge button 
     -- clicks and fire our event with the resulting 
     -- 'WPARAM' values.
     --
     -- First, we need a window
     hwnd <- liftIO $ WG.createWindowEx 0 
          windowClass
          "A mirror"
          (WG.wS_OVERLAPPEDWINDOW + WG.wS_VISIBLE)
          Nothing Nothing
          (Just 500) (Just 500)
          Nothing Nothing
          hinstance
          (wndProcMain (eventLoop refETrigger b))
     
     -- then, we need to create the button to click:
     _ <- liftIO $ WG.createButton "Click me!" (WG.wS_CHILD + WG.wS_VISIBLE) WG.bS_PUSHBUTTON
       (Just 20) (Just 20) (Just 75) (Just 20)
       (Just hwnd) Nothing hinstance
 
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

eventLoop :: (Show a)
          => IORef (Maybe (RH.EventTrigger (R.SpiderTimeline R.Global) WG.WPARAM))
          -> R.Behavior (R.SpiderTimeline R.Global) a
          -> WG.WPARAM
          -> IO ()
eventLoop refETrigger b input = 
     -- this works because SpiderTimeline Global has no data; 
     -- it is the IORef and the Behavior which are interesting.
     R.runSpiderHost $ do

        meTrigger <- liftIO $ readIORef refETrigger
        CM.forM_ meTrigger $ \eTrigger -> do
          RH.fireEvents [eTrigger :=> CMI.Identity input]
        output <- RH.runHostFrame $ R.sample b

        liftIO $ putStrLn $ "Output behavior: " ++ show output

main :: IO ()
main = do
   let winClass = WG.mkClassName "standard-window-1"
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
