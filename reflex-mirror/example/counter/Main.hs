{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main, counterWindow) where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import qualified Data.Default
import qualified Data.Map
import Data.Maybe (isJust)
import qualified Foreign.Ptr
import qualified Graphics.Win32
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
import qualified Graphics.Win32.Toolbar
import Data.Word (Word16)

main :: IO ()
main = do
  -- this is suboptimal; the native Win32 entrypoint passes the Module handle in
  -- but in the absense of a a mechanism to pass it from the native entry point
  -- to the haskell entry point, we get the main instance handle. this is actually
  -- the one we are expecting anyway, so it's no big deal; it just makes Raymond
  -- Chan unhappy.
  mainInstance <- System.Win32.getModuleHandle Nothing

  -- we load some icons. the icon that is actually in use while running the
  -- program will depend on the user's dpi. they are a little ... creepy; but
  -- that is the only way i know how to draw.
  let loadIcon size = Graphics.Win32.c_LoadImage mainInstance Resources.idiAppIcon Graphics.Win32.iMAGE_ICON size size 0

  icon16 <- loadIcon 16
  icon24 <- loadIcon 24
  icon32 <- loadIcon 32
  icon48 <- loadIcon 48
  icon64 <- loadIcon 64
  let icons = IconCollection {..}

  -- now we have to register a window class to be able to run.
  -- note that the standard haskell Win32 bindings have some unexpected wrapper for
  -- the window proc.
  -- but i do not want its features, so we register a class with registerClassEx (which doesn't
  -- have this feature) and create the window with c_CreateWindowEx inside Reflex.Win32.Window.
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
  () <$ Graphics.Win32.registerClassEx wndClass -- we don't use the atom it produces
  System.Win32.Dpi.withDpiFunctions $ \dpiFunctions ->
    Reflex.Win32.Host.host (Reflex.Win32.Window.runWindow $ counterWindow icons winClass dpiFunctions)

counterWindow ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  IconCollection ->
  Graphics.Win32.ClassName ->
  System.Win32.Dpi.DpiFunctions ->
  Reflex.Win32.Window.Win32Window t m (Reflex.Event t ())
counterWindow icons winClass dpiFunctions = mdo
  dpi <- Reflex.Win32.Dpi.dpiControl dpiFunctions windowOut

  (windowOut, windowSizeOut, (dCount, eQuit)) <-
    mainWindow winClass dCount (Reflex.Win32.Dpi.dpiResponder dpi) icons (Reflex.Win32.Dpi.dDpi dpi) $
      counter winClass dpiFunctions dpi (Reflex.Win32.Window.dwoClientSizeInfo windowSizeOut)

  return $ Reflex.leftmost 
   [ () <$ Reflex.Win32.Window.ewoMessage windowOut Graphics.Win32.wM_CLOSE
   , eQuit
   ]

counter ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Graphics.Win32.ClassName ->
  System.Win32.Dpi.DpiFunctions ->
  Reflex.Win32.Dpi.DpiOut t ->
  Reflex.Dynamic t (Int, Int) ->
  Reflex.Win32.Window.Win32Window t m (Reflex.Dynamic t Int, Reflex.Event t ())
counter clsName dpiFunctions Reflex.Win32.Dpi.DpiOut {..} parentDims = mdo
  -- we have a toolbar with two buttons, one to reset and the other to exit - it behaves the same as clicking close.
  (tbOut, (eReset, eQuit)) <- toolbar dNominalPixelsWord16

  -- we want to use the modern font, scaled to the current dpi, on our buttons rather than the default
  dHfont <- Reflex.Win32.ThemeFont.themeFont dpiFunctions dDpi

  -- now we set up a secondary window, sized to begin from the bottom of the toolbar.
  let size = Reflex.Win32.Window.WindowSizeInCodeControlled $ do
          (_, _, _, y) <- Reflex.Win32.Toolbar.dtboSize tbOut
          (w, h') <- parentDims
          return (0, fromIntegral y, fromIntegral w, fromIntegral $ h'-y)

  -- it paints a box.
  let painter' = painter count

  (wo, _, count) <- Reflex.Win32.Window.windowCodeSized createWindowParams (windowIn painter') size $ mdo
     -- core of the counter. a down button that exists whenever there count is positive
     -- an up button that always exists.
     -- a count that increments when the up button is pressed and decrements when down is pressed
     let downButton = button (pure "Down") dHfont [(npx 10, npx 10, npx 70, npx 30) | npx <- dNominalPixels]
     dCountIsPositive <- Reflex.holdUniqDyn ((> 0) <$> count)
     let eEnableDownButton = Reflex.ffilter id $ Reflex.updated dCountIsPositive
     let eDisableDownButton = Reflex.ffilter not $ Reflex.updated dCountIsPositive
     let blank = return Reflex.never
     eDown <- Reflex.Network.networkHold blank $ Reflex.leftmost [downButton <$ eEnableDownButton, blank <$ eDisableDownButton]
     eUp <- button (pure "Up") dHfont [(npx 10, npx 50, npx 70, npx 30) | npx <- dNominalPixels]
     let eChangeCount = Reflex.leftmost [(+1) <$ eUp, flip (-) 1 <$ Reflex.switchDyn eDown, 
             const 0 <$ eReset]
     count <- Reflex.foldDyn ($) 0 eChangeCount
     return count

  dHwnd <- Reflex.holdDyn Nothing (Reflex.Win32.Window.ewoHwnd wo)
  let eHwndOnNewPainter = Reflex.current dHwnd `Reflex.tag` Reflex.updated painter'
  -- we do not want to send Nothing into invalidateRect, since it will cause every window
  -- from every program to be invalidated.
  Reflex.performEvent_ $ invalidateWindow <$> Reflex.ffilter isJust eHwndOnNewPainter

  -- we send the count up to the parent, since the colored box is implemented in WM_PAINT
  return (count, eQuit)

  where
    dBoxPos = [(npx 90, npx 10, npx 160, npx 80) | npx <- dNominalPixelsInt32]

    -- causes the new paint function to be used
    invalidateWindow hwnd = liftIO $ Graphics.Win32.invalidateRect hwnd Nothing True

    -- these are basically the parameters to c_CreateWindowEx, altho certain
    -- styles have been split (when they are changed with a specific api and I
    -- have written code to support that. also dwiHfont is added since it seems
    -- common enough to want to set the font.
    createWindowParams =
      Reflex.Win32.Window.CreateWindowParams
        { Reflex.Win32.Window.dwiWindowStyleEx = pure 0,
          Reflex.Win32.Window.cwiClass = Reflex.Win32.Window.RegisteredClassName clsName,
          Reflex.Win32.Window.dwiTitle = pure "",
          Reflex.Win32.Window.dwiWindowStyle = pure (Graphics.Win32.wS_CHILD .|. Graphics.Win32.wS_CLIPCHILDREN),
          Reflex.Win32.Window.dwiWsExTopmost = pure False,
          Reflex.Win32.Window.dwiWsVisible = pure True,
          Reflex.Win32.Window.dwiHfont = Nothing
        }
    painter dCount = do
      -- i should probably change responders to run in HostFrame t () so they can be sampled directly
      -- rather than recreating a closure each time they change (and then i could use more Behaviors)
      count <- dCount
      boxPos <- dBoxPos
      return $ Data.Map.singleton Graphics.Win32.wM_PAINT $
        \hwnd _wparam _lparam -> Just 0 <$ do
          -- we get a color from a System Color, that you might have set up in the
          -- old Windows95 Appearance dialog box.
          let countMod31 = count `mod` 31 -- defined colors are 0-31 except 25
          let sysColor
                | count < 0 = System.Win32.cOLOR_WINDOW
                | countMod31 == 25 = System.Win32.cOLOR_WINDOW
                | otherwise = fromIntegral countMod31
          color <- getSysColor sysColor

          -- then we create a GDI brush
          brush <- Graphics.Win32.createSolidBrush color

          -- and paint a rectangle using that brush at our defined location.
          Graphics.Win32.allocaPAINTSTRUCT $ \lpps ->
            bracket (Graphics.Win32.beginPaint hwnd lpps) (Graphics.Win32.endPaint lpps) $ \hdc ->
              Graphics.Win32.fillRect hdc boxPos brush

    -- you could set dwiResponse if you want to return something in response to
    -- a window message, or in general if you want to do something before the
    -- window procedure returns. i commonly use it for WM_PAINT and WM_DPICHANGED.
    windowIn painter' =
      Reflex.Win32.Window.WindowIn
        { Reflex.Win32.Window.dwiResponse = painter',
          Reflex.Win32.Window.dwiEarlyResponder = pure $ \_ _ _ _ -> return Nothing
        }

toolbar ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Dynamic t (Int -> Word16) ->
  Reflex.Win32.Window.Win32Window t m
    ( Reflex.Win32.Toolbar.ToolbarOut t,
      ( Reflex.Event t (),
        Reflex.Event t ()
      )
    )
toolbar dNominalPixelsWord16 = Reflex.Win32.Toolbar.toolbar toolbarParams $ do
    -- the first toolbar button uses the standard "File New" icon just because we can.
    btnReset <- Reflex.Win32.Toolbar.toolbarButton 
       (Win32.Utils.Toolbar.StandardImage Graphics.Win32.Toolbar.iDB_STD_LARGE_COLOR Graphics.Win32.Toolbar.sTD_FILENEW)
       Graphics.Win32.Toolbar.tBSTATE_ENABLED
       (Graphics.Win32.Toolbar.bTNS_SHOWTEXT .|. Graphics.Win32.Toolbar.bTNS_AUTOSIZE)
       "Reset"

    -- the second toolbar button just says "Exit" without an image.
    btnExit <- Reflex.Win32.Toolbar.toolbarButton 
       Win32.Utils.Toolbar.NoImage
       Graphics.Win32.Toolbar.tBSTATE_ENABLED
       (Graphics.Win32.Toolbar.bTNS_SHOWTEXT .|. Graphics.Win32.Toolbar.bTNS_AUTOSIZE)
       "Exit"
    return (Win32.Utils.Toolbar.tbClick btnReset, Win32.Utils.Toolbar.tbClick btnExit)
  where
    toolbarParams = Reflex.Win32.Toolbar.ToolbarIn 
        { Reflex.Win32.Toolbar.dtbiBitmapSize = pure (24, 24),  -- fixed, since I am using stock images known to be 24x24
          Reflex.Win32.Toolbar.dtbiButtonSize = [(npx 48, npx 48) | npx <- dNominalPixelsWord16],
          Reflex.Win32.Toolbar.dtbiStyle = pure (Graphics.Win32.Toolbar.tBSTYLE_LIST .|. Graphics.Win32.Toolbar.tBSTYLE_WRAPABLE),
          Reflex.Win32.Toolbar.dtbiVisible = pure True,
          Reflex.Win32.Toolbar.dtbiResponders = pure mempty

      }

mainWindow ::
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Graphics.Win32.ClassName ->
  Reflex.Dynamic t Int ->
  Reflex.Win32.Window.Responder ->
  IconCollection ->
  Reflex.Dynamic t Int ->
  Reflex.Win32.Window.Win32Window t m a ->
  Reflex.Win32.Window.Win32Window
    t
    m
    ( Reflex.Win32.Window.WindowOut t,
      Reflex.Win32.Window.WindowSizeOutUserControlled t,
      a
    )
mainWindow clsName dCount responder icons dDpi children = do
  info <- Reflex.Win32.Window.windowUserSized createWindowParams windowIn size children
  let (wo, _, _) = info
  dHwnd <- Reflex.holdDyn Nothing (Reflex.Win32.Window.ewoHwnd wo)

  -- i want to update the icon based on the dpi so it's always rendered optimally on any screen.
  -- (these icons are gratuitously different to highlight the difference.)
  Reflex.performEvent_ $ updateIcon 0 <$> newIcon dHiconSmall dHwnd
  Reflex.performEvent_ $ updateIcon 1 <$> newIcon dHiconLarge dHwnd

  -- here is an entirely gratutously response to a message.
  let backgroundClicked = Reflex.Win32.Window.ewoMessage wo Graphics.Win32.wM_LBUTTONDOWN
  let backgroundShiftClicked = Reflex.ffilter ((== mK_SHIFT) . Reflex.Win32.Window.wmWparam) backgroundClicked
  Reflex.performEvent_ $ void . showMessage <$> Reflex.current dHwnd `Reflex.tag` backgroundShiftClicked

  return info
  where
    mK_SHIFT = 0x0004
    showMessage hwnd = liftIO $ Graphics.Win32.messageBox hwnd "You shift-clicked the background" "In case you were unaware" 0

    wM_SETICON = 0x0080
    newIcon dIcon dHwnd = Reflex.fmapMaybe sequence $ Reflex.updated $ (,) <$> dIcon <*> dHwnd
    dHiconSmall = smallIconForDpi icons <$> dDpi
    dHiconLarge = largeIconForDpi icons <$> dDpi
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
    -- common enough to want to set the font.
    createWindowParams =
      Reflex.Win32.Window.CreateWindowParams
        { Reflex.Win32.Window.dwiWindowStyleEx = pure 0,
          Reflex.Win32.Window.cwiClass = Reflex.Win32.Window.RegisteredClassName clsName,
          Reflex.Win32.Window.dwiTitle = ["you are up to number " ++ show count | count <- dCount],
          Reflex.Win32.Window.dwiWindowStyle = pure (Graphics.Win32.wS_OVERLAPPEDWINDOW .|. Graphics.Win32.wS_CLIPCHILDREN),
          Reflex.Win32.Window.dwiWsExTopmost = pure False,
          Reflex.Win32.Window.dwiWsVisible = pure True,
          Reflex.Win32.Window.dwiHfont = Nothing
        }

    -- you could set dwiResponse if you want to return something in response to
    -- a window message, or in general if you want to do something before the
    -- window procedure returns. i commonly use it for WM_PAINT and WM_DPICHANGED.
    -- nb. if you use it for WM_PAINT, you probably want to send invalidateRect whenever
    -- the WM_PAINT handler changes. I forget what early responder is about...
    windowIn =
      Reflex.Win32.Window.WindowIn
        { Reflex.Win32.Window.dwiResponse = pure responder,
          Reflex.Win32.Window.dwiEarlyResponder = pure $ \_ _ _ _ -> return Nothing
        }
    -- with a UserSized window, size specifies the initial size and an event which fires
    -- when the code wants to change the size. the actual size is specified in a Dynamic
    -- which returns.
    size =
      Reflex.Win32.Window.WindowSizeInUserControlled
        { Reflex.Win32.Window.cwiInitialPos = (Nothing, Nothing, Just 1500, Just 1300),
          Reflex.Win32.Window.ewiSetPos = Reflex.never
        }

button ::
  forall t m.
  Reflex.Win32.Window.Win32WindowConstraints t m =>
  Reflex.Dynamic t String ->
  Reflex.Dynamic t Graphics.Win32.HFONT ->
  Reflex.Dynamic t (Int, Int, Int, Int) ->
  Reflex.Win32.Window.Win32Window t m (Reflex.Event t ())
button label dHfont pos = do
  -- in the case of a code-sized window, there is no size out information since the dynamic should know it all.
  -- in the case of a window with no children, the return value from the child is () so not interesting.
  -- so the window out messages are the only thing that's interesting.
  (wo, _size, _childOut) <- Reflex.Win32.Window.windowCodeSized createWindowParams windowIn size $ return ()
  return $ Reflex.Win32.Window.click wo
  where
    createWindowParams =
      Reflex.Win32.Window.CreateWindowParams
        { Reflex.Win32.Window.dwiWindowStyleEx = pure 0,
          Reflex.Win32.Window.cwiClass = Reflex.Win32.Window.Button,
          Reflex.Win32.Window.dwiTitle = label,
          Reflex.Win32.Window.dwiWindowStyle = pure (Graphics.Win32.wS_CHILD .|. Graphics.Win32.bS_PUSHBUTTON),
          Reflex.Win32.Window.dwiWsExTopmost = pure False,
          Reflex.Win32.Window.dwiWsVisible = pure True,
          Reflex.Win32.Window.dwiHfont = Just dHfont
        }
    -- these aren't actually used in the case of standard window type
    windowIn = Data.Default.def
    -- with a CodeSized window, size is a dynamic.
    size =
      Reflex.Win32.Window.WindowSizeInCodeControlled
        { Reflex.Win32.Window.dwiPos = pos
        }

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
-- goal/responsibility of the reflex-mirror. therefore, we just import one to throw away
-- disadvantage: we do not receive the WM_CREATE message, because it goes to this
-- before the mirror has a chance to insert its own.

foreign import ccall unsafe "&DefWindowProcW"
  defWndProcFP ::
    Foreign.Ptr.FunPtr Graphics.Win32.WindowClosure

-- this is a function which doesn't seem to exist in Win32 and i don't seem to have wrapped it
-- (i guess because i mostly use GetThemeSysColor)

foreign import ccall unsafe "GetSysColor"
  getSysColor ::
    System.Win32.SystemColor -> IO Graphics.Win32.COLORREF
