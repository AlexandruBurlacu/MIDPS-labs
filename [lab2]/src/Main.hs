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
  st <- newIORef (Value "" Nothing)
  void initGUI
  -- Main window
  window <- windowNew
  set window [ windowTitle         := "Calculator",
               windowDefaultWidth  := 230,
               windowDefaultHeight := 300,
               windowResizable     := False ]

  -- Display for numbers
  display <- entryNew
  set display [ entryEditable := False,
                entryXalign   := 1, -- makes contents right-aligned
                entryText     := "0" ]

  grid <- Grid.gridNew
  Grid.gridSetRowHomogeneous grid True

  let attach x y w h item = Grid.gridAttach grid item x y w h
      mkBtn = mkButton st display

  attach 0 0 5 1 display

  mkBtn "←"   backspace                             >>= attach 0 2 1 1
  mkBtn "CE"  clearEntry                            >>= attach 1 2 1 1
  mkBtn "C"   clearAll                              >>= attach 2 2 1 1
  mkBtn "±"   (operator (\x -> Invert x))           >>= attach 3 2 1 1
  mkBtn "√"   (operator (\x -> SquareRoot x))       >>= attach 4 2 1 1

  mkBtn "7"   (enterDigit '7')                      >>= attach 0 3 1 1
  mkBtn "8"   (enterDigit '8')                      >>= attach 1 3 1 1
  mkBtn "9"   (enterDigit '9')                      >>= attach 2 3 1 1
  mkBtn "÷"   (operator (\x -> Division x))         >>= attach 3 3 1 1
  mkBtn "^"   (operator (\x -> Power x))            >>= attach 4 3 1 1

  mkBtn "4"   (enterDigit '4')                      >>= attach 0 4 1 1
  mkBtn "5"   (enterDigit '5')                      >>= attach 1 4 1 1
  mkBtn "6"   (enterDigit '6')                      >>= attach 2 4 1 1
  mkBtn "*"   (operator (\x -> Multiplication x))   >>= attach 3 4 1 1

  mkBtn "1"   (enterDigit '1')                      >>= attach 0 5 1 1
  mkBtn "2"   (enterDigit '2')                      >>= attach 1 5 1 1
  mkBtn "3"   (enterDigit '3')                      >>= attach 2 5 1 1
  mkBtn "–"   (operator (\x -> Subtraction x))      >>= attach 3 5 1 1
  mkBtn "="   equals                                >>= attach 4 5 1 2

  mkBtn "0"   (enterDigit '0')                      >>= attach 0 6 2 1
  mkBtn "."   enterDot                              >>= attach 2 6 1 1
  mkBtn "+"   (operator (\x -> Addition x))         >>= attach 3 6 1 1

  containerAdd window grid

  window `on` deleteEvent $ do
  -- handler to run on window destruction
    liftIO mainQuit
    return False

  widgetShowAll window
  mainGUI
