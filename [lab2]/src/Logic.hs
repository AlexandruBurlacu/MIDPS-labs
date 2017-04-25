module Logic(
  Value(..),
  Action(..)
) where

-- 'Value' holds textual representation of first argument reversed and
-- 'Action' to apply to it, which see.
data Value = Value String (Maybe Action)

-- Action to apply to first argument and textual representation of second
-- argument reversed (if relevant).
data Action
  = Addition       String
  | Subtraction    String
  | Multiplication String
  | Division       String
  | SquareRoot     String
  | Power          String
  | Invert         String

getSndArg :: Action -> String
-- Get second argument from 'Action'.
getSndArg (Addition       x) = x
getSndArg (Subtraction    x) = x
getSndArg (Multiplication x) = x
getSndArg (Division       x) = x
getSndArg (SquareRoot     x) = x
getSndArg (Power          x) = x
getSndArg (Invert         x) = x

