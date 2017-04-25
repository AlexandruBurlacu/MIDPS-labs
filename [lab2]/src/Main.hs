module Main where

-- Additional Imports
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import qualified Graphics.UI.Gtk.Layout.Grid as Grid

-- 'Business' Logic Module
import Logic

updateDisplay :: Entry -> Value -> IO ()
-- Make calculator's display show given 'Value'.
updateDisplay display value =
  set display [ entryText := renderValue value ]


mkButton
  :: IORef Value       -- ^ 'IORef' to calculator state
  -> Entry             -- ^ Our display to update
  -> String            -- ^ Button label
  -> (Value -> Value)  -- ^ How this button affects calculator state
  -> IO Button         -- ^ Resulting button object
-- Create a button and attach handler to it that mutates calculator's
-- state with given function.
mkButton st display label mutateState = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r)
    updateDisplay display value
  return btn


main :: IO ()
main = do
  void initGUI
  -- Main window
  window <- windowNew
  set window [ windowTitle         := "Calculator",
               windowDefaultWidth  := 230,
               windowDefaultHeight := 250,
               windowResizable     := True ]
  -- Display for numbers
  display <- entryNew
  set display [ entryEditable := False,
                entryXalign   := 1, -- makes contents right-aligned
                entryText     := "0" ]

  grid <- Grid.gridNew
  Grid.gridSetRowHomogeneous grid True

  window `on` deleteEvent $ do
  -- handler to run on window destruction
    liftIO mainQuit
    return False

  widgetShowAll window
  mainGUI
