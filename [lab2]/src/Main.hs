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
  putStrLn "Let the IDE Lab #2 begin!"
  putStrLn "It's evolving :D"
