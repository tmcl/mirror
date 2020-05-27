{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main, counterWindow) where

import Prelude hiding (print, putStrLn)
import Control.Exception (bracket)
import Control.Monad (void)
import qualified Control.Monad.Fix
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader
import Control.Monad.Reader (ask, asks)
import Data.Bits ((.&.), (.|.))
import qualified Data.Default
import qualified Data.Map
import Data.Maybe (isJust)
import qualified Foreign.Ptr
import qualified Graphics.Win32
import qualified Graphics.Win32.Toolbar
import qualified Reflex
import qualified Reflex.Network
import qualified Reflex.Win32.Dpi
import qualified Reflex.Win32.Host
import qualified Reflex.Win32.ThemeFont
import qualified Reflex.Win32.Toolbar
import qualified Reflex.Win32.Window
import qualified Resources
import qualified System.Win32
import qualified System.Win32.Dpi
import qualified Win32.Utils.Toolbar

-- welcome to Reflex.Win32. It is much lower-level than Reflex.DOM,
-- partly since WINAPI is lower level than DOM, and partly since it's
-- still wip, newer, younger etc.
--
-- this gives us a program which:

-- * opens a window

-- * has one or two buttons in its body, labelled Up and Down

-- ** the Down button is optional. it is only present if the count is greater
--    than zero

-- ** the Up button is always present

-- ** the Down button decrements the count by one

-- ** the Up button increments the count by one

-- * the current count is presented in the window title

-- * moreover, a color is derived from the count (by using the SystemColor enum
--   which records the colors you could configure in the old Windows Appearance
--   control panel)

-- ** a box is painted in the color of this derived color.

-- * the window contains a toolbar

-- ** the toolbar contains two buttons: Reset, and Exit.

-- ** Reset resets the count to 0

-- ** Exit quits the program, and is fully equivalent to clicking the close
--    button

-- * all widgets scale according to dpi (using Windows 10 apis)

-- * all widgets use the modern, dpi-scaled system font

-- * even the system icons scale according to dpi;

-- ** the task bar/alt-tab icon changes

-- ** the small icon in the titlebar changes

-- * moreover, purely for the sake of it, you can shift-click the background
--   and it will respond

main :: IO ()
main = do
  -- we need to initialise our windows program. ghc forgets the main module
  -- handle, so we get the global module handle. this is perfectly fine for our
  -- purposes, but we shouldn't call this function more than once

  mainInstance <- System.Win32.getModuleHandle Nothing

  -- we load some icons. the icons that are actually in use while running the
  -- program will depend on the user's dpi. they are a little ... creepy; but
  -- that is the only way i know how to draw.
  let loadIcon size =
        Graphics.Win32.c_LoadImage
          mainInstance
          Resources.idiAppIcon
          Graphics.Win32.iMAGE_ICON
          size
          size
          0

  icon16 <- loadIcon 16
  icon24 <- loadIcon 24
  icon32 <- loadIcon 32
  icon48 <- loadIcon 48
  icon64 <- loadIcon 64
  let icons = IconCollection {..}

  -- now we have to register a window class to be able to run.
  -- note that the standard haskell Win32 bindings have some unexpected wrapper
  -- for the window proc (it provides a c window procedure that delegates to a
  -- function pointer stored in the windows' user data).
  -- but i do not want its features (reflex-mirror provides its own window
  -- procedure), so we register a class with registerClassEx (which doesn't
  -- have this feature) and create the window with c_CreateWindowEx inside
  -- Reflex.Win32.Window.
  cursor <- Graphics.Win32.loadCursor Nothing Graphics.Win32.iDC_ARROW
  let winClass = Graphics.Win32.mkClassName "example-counter"
  let wndClass =
        Graphics.Win32.WNDCLASSEXW
          { wxStyle = Graphics.Win32.cS_VREDRAW + Graphics.Win32.cS_HREDRAW,
            wxWndProc = defWndProcFP,
            wxHInstance = mainInstance,
            wxHIcon = Just icon32,
            wxHIconSmall = Just icon16,
            wxHCursor = Just cursor,
            wxBackground = Just $ Right System.Win32.cOLOR_WINDOW,
            wxMenuName = Nothing,
            wxClassName = winClass,
            wxClassExtra = 0,
            wxWindowExtra = 0
          }
  () <$ Graphics.Win32.registerClassEx wndClass -- we won't unregister it.

  -- we want to get the high dpi functions. this little wrapper is provided,
  -- rather than globals, because the version of mingw GHC 8.8.3 ships with
  -- doesn't know about the dpi functions. so we dynamically bind to them at
  -- runtime
  System.Win32.Dpi.withDpiFunctions $ \dpiFunctions ->
    -- and now we run our program.
    -- "host" starts a message loop;
    -- "runWindow" provides enough information to be able
    -- to start a top-level window.
    Reflex.Win32.Host.host
      ( Reflex.Win32.Window.runWindow $
          counterWindow icons winClass dpiFunctions
      )

counterWindow ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  IconCollection ->
  Graphics.Win32.ClassName ->
  System.Win32.Dpi.DpiFunctions ->
  Reflex.Win32.Window.Win32Window t m (Reflex.Event t ())
counterWindow icons winClass dpiFunctions = mdo
  -- so our counter window is a main window with
  -- a certain icons, windowclass, dpi functions,
  -- a dynamic title, and it contains "counter"
  -- as its children
  (windowOut, (dTitle, eQuit)) <-
    mainWindow icons winClass dpiFunctions dTitle counter

  -- we finish our program when we get the WM_CLOSE message
  -- or when the "eQuit" event is triggered, which happens
  -- to be when the user clicks the "Exit" toolbar button.
  return $
    Reflex.leftmost
      [ () <$ Reflex.Win32.Window.ewoMessage windowOut Graphics.Win32.wM_CLOSE,
        eQuit
      ]

-- here is the business logic of our app. to comfortably separate the
-- business logic and the implementation detail we use a custom monad,
-- but we'll get into the detail of that later. this counter returns
-- the current desired window title and an event which fires when it's
-- time to quit (it's "eQuit" in "counterWindow" above).
counter ::
  forall t m.
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  AppT t m (Reflex.Dynamic t String, Reflex.Event t ())
counter = mdo
  -- we have a toolbar with two buttons, one to reset and the other to exit -
  -- it behaves the same as clicking close.
  let tbButtons = ToolbarButtons <$> tbButtonReset <*> tbButtonExit

  -- we paint a box. the box uses the current count (note: we use `mdo`,
  -- since this is an input to the "dpiControlledToolbarWindow" but
  -- depends on its output - namely, the current count). the exact
  -- size and position of the box depend on the current dpi
  dNominalPixelsInt32 <-
    asks
      (Reflex.Win32.Dpi.dNominalPixelsInt32 . dDpiStatus)
  let dBoxPos = [(npx 90, npx 10, npx 160, npx 80) | npx <- dNominalPixelsInt32]
  let painter = mkPainter dCount dBoxPos

  -- "dpiControlledToolbarWindow" is something we've written to tie the
  -- parts of our app together it knows of our painter
  (ToolbarButtons eReset eQuit, dCount) <- dpiControlledToolbarWindow
    painter
    tbButtons
    $ mdo
      count <- do
        -- core of the counter:
        --   * a down button that exists whenever there count is positive.
        --   * an up button that always exists.
        --   * a count that increments when the up button is pressed and 
        --     decrements when down is pressed
        --   * whenever the reset button is clicked, we go back to 0
        --   * and we fold them all together to get the current count
        dCountIsPositive <- Reflex.holdUniqDyn ((> 0) <$> count)
        eDown <- downButton dCountIsPositive
        eUp <- upButton
        let eChangeCount =
              Reflex.leftmost
                [ (+ 1) <$ eUp,
                  (+ (-1)) <$ eDown,
                  const 0 <$ eReset
                ]
        Reflex.foldDyn ($) 0 eChangeCount
      return count

  -- the window title shows a message based on the current count
  let title = ["you are up to number " ++ show count | count <- dCount]
  return (title, eQuit)
  where
    -- the painter. i might still want to change some of the implementation
    -- detail here
    mkPainter ::
      Reflex.Dynamic t Int ->
      Reflex.Dynamic t Graphics.Win32.RECT ->
      Reflex.Dynamic t Reflex.Win32.Window.Responder
    mkPainter dCount dBoxPos = do
      -- at the moment, we sample the dynamics and produce a new painter
      -- each time they change.
      -- i should probably change responders to run in HostFrame t () so they
      -- can be sampled directly rather than recreating a closure each time
      -- they change (and then i could use more Behaviors)
      count <- dCount
      boxPos <- dBoxPos
      return $ Data.Map.singleton Graphics.Win32.wM_PAINT $
        -- WM_PAINT does not provide any interesting values in wparam or lparam
        \hwnd _wparam _lparam -> Just 0 <$ do
          -- we get a color from a System Color, that you might have set up in
          -- the old Windows95 Appearance dialog box.
          let countMod31 = count `mod` 31 -- defined colors are 0-30 except 25
          let sysColor
                | count < 0 = System.Win32.cOLOR_WINDOW
                | countMod31 == 25 = System.Win32.cOLOR_WINDOW
                | otherwise = fromIntegral countMod31
          color <- getSysColor sysColor

          -- then we create a GDI brush
          brush <- Graphics.Win32.createSolidBrush color

          -- and paint a rectangle using that brush at our defined location.
          Graphics.Win32.allocaPAINTSTRUCT $ \lpps ->
            bracket
              (Graphics.Win32.beginPaint hwnd lpps)
              (Graphics.Win32.endPaint lpps)
              $ \hdc ->
                Graphics.Win32.fillRect hdc boxPos brush

-- this is convenient (i think it makes the code a bit easier to understand),
-- but I could just use a tuple. we access the fields by pattern matching
data ToolbarButtons t = ToolbarButtons
  { _tbbReset :: Reflex.Event t (),
    _tbbQuit :: Reflex.Event t ()
  }

-- that's all the interesting stuff in our app. the rest is implementation
-- detail.

-- we have our own MonadReader as mentioned to help separate the implementation
-- from the business logic. some of the stuff stored here is stuff that might
-- change over the life of the app, but it's stuff we don't really want to
-- care about. and then we have some boiler plate to make it work.

data AppEnv t = AppEnv
  { className :: Graphics.Win32.ClassName,
    dDpiStatus :: Reflex.Win32.Dpi.DpiOut t,
    dHFont :: Reflex.Dynamic t Graphics.Win32.HFONT,
    parentDims :: Reflex.Dynamic t (Int, Int)
  }

newtype AppT t m a = AppT
  { unApp ::
      Control.Monad.Reader.ReaderT
        (AppEnv t)
        (Reflex.Win32.Window.Win32Window t m)
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      Control.Monad.Reader.MonadReader (AppEnv t),
      Control.Monad.Fix.MonadFix,
      Reflex.MonadSample t,
      Reflex.MonadHold t,
      Reflex.PostBuild t,
      Reflex.NotReady t
    )

deriving instance
  (Reflex.Win32.Window.Win32WindowConstraints t m) =>
  Reflex.PerformEvent t (AppT t m)

instance
  (Reflex.Win32.Window.Win32WindowConstraints t m) =>
  Reflex.Adjustable t (AppT t m)
  where
  runWithReplace ma0 ema = do
    appEnv <- ask
    let ma0' = runApp appEnv ma0
    let ema' = runApp appEnv <$> ema
    AppT . Control.Monad.Reader.lift $ Reflex.runWithReplace ma0' ema'

  -- i don't use these, and the base library doesn't implement them yet.
  traverseIntMapWithKeyWithAdjust = undefined
  traverseDMapWithKeyWithAdjustWithMove _ = undefined

runApp :: AppEnv t -> AppT t m a -> Reflex.Win32.Window.Win32Window t m a
runApp appEnv = flip Control.Monad.Reader.runReaderT appEnv . unApp

-- okay, back to what's interesting.

-- first, some widgets which exist purely for our app and would be generally
-- useless to other apps: up & down buttons; reset & exit toolbar buttons

-- our up button has a certain location and is labelled "Up"
-- the location is specified in dpi independent pixels because of the code in
-- "button"
upButton ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  AppT t m (Reflex.Event t ())
upButton = button (pure "Up") (10, 50, 70, 30)

-- our down button has a certain location and is labelled "Down"
-- we take advantage of MonadAdjust here, since the definition
-- of "visible" that we're using is actually "exists at all"
downButton ::
  forall t m.
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Dynamic t Bool ->
  AppT t m (Reflex.Event t ())
downButton dIsVisible = do
  let dWidget = do
        isVisible <- dIsVisible
        if not isVisible
          then return $ return Reflex.never
          else return $ button (pure "Down") (10, 10, 70, 30)

  -- (Reflex.Network.networkView is equivalent to dyn in reflex-dom)
  r <- Reflex.Network.networkView dWidget
  Reflex.switchHoldPromptly Reflex.never r

-- the first toolbar button uses the standard "File New" icon just because we
-- can.
tbButtonReset ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Win32.Toolbar.Win32Toolbar
    t
    (Reflex.Win32.Host.Win32Guest t m)
    (Reflex.Event t ())
tbButtonReset =
  Win32.Utils.Toolbar.tbClick
    <$> Reflex.Win32.Toolbar.toolbarButton
      ( Win32.Utils.Toolbar.StandardImage
          Graphics.Win32.Toolbar.iDB_STD_LARGE_COLOR
          Graphics.Win32.Toolbar.sTD_FILENEW
      )
      Graphics.Win32.Toolbar.tBSTATE_ENABLED
      ( Graphics.Win32.Toolbar.bTNS_SHOWTEXT
          .|. Graphics.Win32.Toolbar.bTNS_AUTOSIZE
      )
      "Reset"

-- the second toolbar button just says "Exit" without an image.
tbButtonExit ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Win32.Toolbar.Win32Toolbar
    t
    (Reflex.Win32.Host.Win32Guest t m)
    (Reflex.Event t ())
tbButtonExit =
  Win32.Utils.Toolbar.tbClick
    <$> Reflex.Win32.Toolbar.toolbarButton
      Win32.Utils.Toolbar.NoImage
      Graphics.Win32.Toolbar.tBSTATE_ENABLED
      ( Graphics.Win32.Toolbar.bTNS_SHOWTEXT
          .|. Graphics.Win32.Toolbar.bTNS_AUTOSIZE
      )
      "Exit"

-- now, something that is a little more generic: a button and a window with a
-- toolbar.

-- the implementation of our buttons. it uses the current theme font
-- and the current dpi to produce a button.
button ::
  forall t m.
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Dynamic t String ->
  (Int, Int, Int, Int) ->
  AppT t m (Reflex.Event t ())
button label (x, y, w, h) = do
  dHfont <- asks dHFont
  dNominalPixels <- asks (Reflex.Win32.Dpi.dNominalPixels . dDpiStatus)

  (windowOut, _size, _childOut) <-
    AppT . Control.Monad.Reader.lift
      $ Reflex.Win32.Window.windowCodeSized
        (createWindowParams dHfont)
        windowIn
        (size dNominalPixels)
      $ return ()
  return $ Reflex.Win32.Window.click windowOut
  where
    createWindowParams dHfont =
      Reflex.Win32.Window.CreateWindowParams
        { Reflex.Win32.Window.dwiWindowStyleEx = pure 0,
          Reflex.Win32.Window.cwiClass = Reflex.Win32.Window.Button,
          Reflex.Win32.Window.dwiTitle = label,
          Reflex.Win32.Window.dwiWindowStyle =
            pure
              ( Graphics.Win32.wS_CHILD
                  .|. Graphics.Win32.bS_PUSHBUTTON
              ),
          Reflex.Win32.Window.dwiWsExTopmost = pure False,
          Reflex.Win32.Window.dwiWsVisible = pure True,
          Reflex.Win32.Window.dwiHfont = Just dHfont
        }
    -- these aren't actually used in the case of standard window type
    windowIn = Data.Default.def
    -- with a CodeSized window, size is a dynamic.
    size dNominalPixels =
      Reflex.Win32.Window.WindowSizeInCodeControlled
        { Reflex.Win32.Window.dwiPos =
            [(npx x, npx y, npx w, npx h) | npx <- dNominalPixels]
        }

-- the dpi controlled toolbar window is a window with a toolbar and a child
-- area.  the win32 toolbar automatically attaches to the top of the parent
-- window, but it doesn't create a space below it; you need to use coordinates
-- relative to the top of the toolbar. this is a starting place for a window
-- with a child window.
dpiControlledToolbarWindow ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Dynamic t Reflex.Win32.Window.Responder ->
  Reflex.Win32.Toolbar.Win32Toolbar t (Reflex.Win32.Host.Win32Guest t m) a ->
  AppT t m b ->
  AppT t m (a, b)
dpiControlledToolbarWindow painter toolbarButtons children = do
  appEnv <- ask

  -- include/run the toolbar
  (tbOut, tbButtonsOut) <- toolbar toolbarButtons

  -- now we set up a secondary window, sized to begin from the bottom of the
  -- toolbar.
  -- we run our children inside the secondary window.
  let size = Reflex.Win32.Window.WindowSizeInCodeControlled $ do
        (_, _, _, y) <- Reflex.Win32.Toolbar.dtboSize tbOut
        (w, h') <- parentDims appEnv
        return (0, fromIntegral y, fromIntegral w, fromIntegral $ h' - y)

  (windowOut, _windowSizeOut, b) <-
    AppT $ Control.Monad.Reader.lift
      $ Reflex.Win32.Window.windowCodeSized
        (createWindowParams appEnv)
        windowIn
        size
      $ runApp appEnv children

  -- we want to invalidate the window whenever the painter changes. this will
  -- cause the window to automatically be repainted using the new painter
  -- (rather than having to wait for some resize/uncover event).
  dHwnd <- Reflex.holdDyn Nothing (Reflex.Win32.Window.ewoHwnd windowOut)
  let eHwndOnNewPainter =
        Reflex.current dHwnd
          `Reflex.tag` Reflex.updated painter
  -- we do not want to send Nothing into invalidateRect, since it will cause
  -- every window from every program to be invalidated.
  Reflex.performEvent_ $
    invalidateWindow
      <$> Reflex.ffilter isJust eHwndOnNewPainter

  -- here is an entirely gratuitous response to a message.
  let backgroundClicked =
        Reflex.Win32.Window.ewoMessage
          windowOut
          Graphics.Win32.wM_LBUTTONDOWN
  let backgroundShiftClicked = Reflex.ffilter isShiftClick backgroundClicked
  Reflex.performEvent_ $
    void . showMessage
      <$> Reflex.current dHwnd `Reflex.tag` backgroundShiftClicked

  return (tbButtonsOut, b)
  where
    -- causes the new paint function to be used
    invalidateWindow hwnd =
      liftIO $
        Graphics.Win32.invalidateRect hwnd Nothing True
    -- this is some constant defined by the windows api but not defined in Win32... todo. copy it thence
    mK_SHIFT = 0x0004
    isShiftClick =
      ((== mK_SHIFT) . (.&.) mK_SHIFT)
        . Reflex.Win32.Window.wmWparam
    showMessage hwnd =
      liftIO $
        Graphics.Win32.messageBox
          hwnd
          "You shift-clicked the background"
          "In case you were unaware"
          0
    -- these are basically the parameters to c_CreateWindowEx, altho certain
    -- styles have been split (when they are changed with a specific api and I
    -- have written code to support that. also dwiHfont is added since it seems
    -- common enough to want to set the font.
    createWindowParams appEnv =
      Reflex.Win32.Window.CreateWindowParams
        { Reflex.Win32.Window.dwiWindowStyleEx = pure 0,
          Reflex.Win32.Window.cwiClass =
            Reflex.Win32.Window.RegisteredClassName $ className appEnv,
          Reflex.Win32.Window.dwiTitle = pure "",
          Reflex.Win32.Window.dwiWindowStyle =
            pure (Graphics.Win32.wS_CHILD .|. Graphics.Win32.wS_CLIPCHILDREN),
          Reflex.Win32.Window.dwiWsExTopmost = pure False,
          Reflex.Win32.Window.dwiWsVisible = pure True,
          Reflex.Win32.Window.dwiHfont = Nothing
        }
    -- you could set dwiResponse if you want to return something in response to
    -- a window message, or in general if you want to do something before the
    -- window procedure returns. i commonly use it for WM_PAINT and
    -- WM_DPICHANGED. dwiEarlyResponder is the same, it just happens before
    -- children are processed.
    windowIn =
      Reflex.Win32.Window.WindowIn
        { Reflex.Win32.Window.dwiResponse = painter,
          Reflex.Win32.Window.dwiEarlyResponder = 
            pure $ \_ _ _ _ -> return Nothing
        }

--simple wrapper around the genernic "Reflex.Win32.Toolbar.toolbar" to help
--with dpi status and to set the dull and boring input properties.
toolbar ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Win32.Toolbar.Win32Toolbar t (Reflex.Win32.Host.Win32Guest t m) a ->
  AppT t m (Reflex.Win32.Toolbar.ToolbarOut t, a)
toolbar children = do
  dNominalPixelsWord16 <-
    asks
      (Reflex.Win32.Dpi.dNominalPixelsWord16 . dDpiStatus)
  AppT . Control.Monad.Reader.lift $
    Reflex.Win32.Toolbar.toolbar (toolbarParams dNominalPixelsWord16) children
  where
    toolbarParams dNominalPixelsWord16 =
      Reflex.Win32.Toolbar.ToolbarIn
        { -- fixed size, since I am using stock images known to be 24x24
          Reflex.Win32.Toolbar.dtbiBitmapSize = pure (24, 24),
          Reflex.Win32.Toolbar.dtbiButtonSize =
            [(npx 48, npx 48) | npx <- dNominalPixelsWord16],
          Reflex.Win32.Toolbar.dtbiStyle =
            pure
              ( Graphics.Win32.Toolbar.tBSTYLE_LIST
                  .|. Graphics.Win32.Toolbar.tBSTYLE_WRAPABLE
              ),
          Reflex.Win32.Toolbar.dtbiVisible = pure True,
          Reflex.Win32.Toolbar.dtbiResponders = pure mempty
        }

-- this is the main, toplevel window. it takes in a dynamic title as well, and
-- sets us up for dpi independence, loads the new font, and takes responsibility
-- for replacing its own icons in accordance with dpi changes.
mainWindow ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  IconCollection ->
  Graphics.Win32.ClassName ->
  System.Win32.Dpi.DpiFunctions ->
  Reflex.Dynamic t String ->
  AppT t m a ->
  Reflex.Win32.Window.Win32Window
    t
    m
    ( Reflex.Win32.Window.WindowOut t,
      a
    )
mainWindow icons clsName dpiFunctions dTitle children = do
  rec -- standard app environment. dpiStatus, dHfont and windowSizeOut are
      -- defined later.
      let appEnv =
            AppEnv
              clsName
              dpiStatus
              dHfont
              (Reflex.Win32.Window.dwoClientSizeInfo windowSizeOut)

      -- run the window with children in it.
      -- responder is provided by the dpiControl widget; it is primarily
      -- responsible for updating the window size in response to a dpi change
      -- (since windows behaves nicer if you change the size before you return
      -- from the WM_DPICHANGED window message)
      info <-
        Reflex.Win32.Window.windowUserSized
          createWindowParams
          (windowIn responder)
          size
          $ runApp appEnv children
      let (windowOut, windowSizeOut, childrenOut) = info

      -- dpi control widget
      dpiStatus <- Reflex.Win32.Dpi.dpiControl dpiFunctions windowOut
      let responder = Reflex.Win32.Dpi.dpiResponder dpiStatus
      let dDpi = Reflex.Win32.Dpi.dDpi dpiStatus

      -- we want to use the modern font, scaled to the current dpi, on our
      -- buttons rather than the default
      dHfont <- Reflex.Win32.ThemeFont.themeFont dpiFunctions dDpi

  -- keep track of the window handle so we can use it.
  dHwnd <- Reflex.holdDyn Nothing (Reflex.Win32.Window.ewoHwnd windowOut)

  -- i want to update the icon based on the dpi so it's always rendered
  -- optimally on any screen.
  -- (these icons are gratuitously different to highlight the difference.)
  Reflex.performEvent_ $ updateIcon 0 <$> newIcon (dHiconSmall dDpi) dHwnd
  Reflex.performEvent_ $ updateIcon 1 <$> newIcon (dHiconLarge dDpi) dHwnd

  return (windowOut, childrenOut)
  where
    -- another constant that should be in Win32.
    wM_SETICON = 0x0080
    -- update the icon
    newIcon dIcon dHwnd =
      Reflex.fmapMaybe sequence $ Reflex.updated $
        (,) <$> dIcon <*> dHwnd
    dHiconSmall dDpi = smallIconForDpi icons <$> dDpi
    dHiconLarge dDpi = largeIconForDpi icons <$> dDpi
    updateIcon sizeCode (hicon, hwnd) =
      liftIO $
        Graphics.Win32.postMessage
          (Just hwnd)
          wM_SETICON
          sizeCode
          (fromIntegral $ Foreign.Ptr.ptrToIntPtr hicon)
    -- these are basically the parameters to c_CreateWindowEx, altho certain
    -- styles have been split (when they are changed with a specific api and I
    -- have written code to support that. also dwiHfont is added since it seems
    -- common enough to want to set the font (mostly for control windows)
    createWindowParams =
      Reflex.Win32.Window.CreateWindowParams
        { Reflex.Win32.Window.dwiWindowStyleEx = pure 0,
          Reflex.Win32.Window.cwiClass =
            Reflex.Win32.Window.RegisteredClassName clsName,
          Reflex.Win32.Window.dwiTitle = dTitle,
          Reflex.Win32.Window.dwiWindowStyle =
            pure
              ( Graphics.Win32.wS_OVERLAPPEDWINDOW
                  .|. Graphics.Win32.wS_CLIPCHILDREN
              ),
          Reflex.Win32.Window.dwiWsExTopmost = pure False,
          Reflex.Win32.Window.dwiWsVisible = pure True,
          Reflex.Win32.Window.dwiHfont = Nothing
        }
    -- you could set dwiResponse if you want to return something in response to
    -- a window message, or in general if you want to do something before the
    -- window procedure returns. i commonly use it for WM_PAINT and
    -- WM_DPICHANGED.
    -- nb. if you use it for WM_PAINT, you probably want to send invalidateRect
    -- whenever the WM_PAINT handler changes.
    -- I forget what early responder is about...
    windowIn responder =
      Reflex.Win32.Window.WindowIn
        { Reflex.Win32.Window.dwiResponse = pure responder,
          Reflex.Win32.Window.dwiEarlyResponder = 
             pure $ \_ _ _ _ -> return Nothing
        }
    -- with a UserSized window, size specifies the initial size and an event
    -- which fires when the code wants to change the size. the actual size is
    -- specified in a Dynamic which returns.
    size =
      Reflex.Win32.Window.WindowSizeInUserControlled
        { Reflex.Win32.Window.cwiInitialPos =
            (Nothing, Nothing, Just 1500, Just 1300),
          Reflex.Win32.Window.ewiSetPos = Reflex.never
        }

-- some utilities for icons.
data IconCollection = IconCollection
  { icon16 :: Graphics.Win32.HICON,
    icon24 :: Graphics.Win32.HICON,
    icon32 :: Graphics.Win32.HICON,
    icon48 :: Graphics.Win32.HICON,
    icon64 :: Graphics.Win32.HICON
  }

smallIconForDpi :: IconCollection -> Int -> Graphics.Win32.HICON
smallIconForDpi icons dpi
  | dpi < 120 = icon16 icons
  | dpi > 160 = icon32 icons
  | otherwise = icon24 icons

largeIconForDpi :: IconCollection -> Int -> Graphics.Win32.HICON
largeIconForDpi icons dpi
  | dpi < 120 = icon32 icons
  | dpi > 160 = icon64 icons
  | otherwise = icon48 icons

-- we have no interest in writing a window procedure; that is the
-- goal/responsibility of the reflex-mirror. therefore, we just import one to
-- throw away disadvantage: we do not receive the WM_CREATE message, because it
-- goes to this before the mirror has a chance to insert its own.

foreign import ccall unsafe "&DefWindowProcW"
  defWndProcFP ::
    Foreign.Ptr.FunPtr Graphics.Win32.WindowClosure

-- this is a function which doesn't seem to exist in Win32 and i don't seem to
-- have wrapped it (i guess because i mostly use GetThemeSysColor)

foreign import ccall unsafe "GetSysColor"
  getSysColor ::
    System.Win32.SystemColor -> IO Graphics.Win32.COLORREF
