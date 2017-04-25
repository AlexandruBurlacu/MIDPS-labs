module Main where

-- Additional Imports
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import qualified Graphics.UI.Gtk.Layout.Grid as Grid

-- 'Business' Logic Module
import Logic

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
