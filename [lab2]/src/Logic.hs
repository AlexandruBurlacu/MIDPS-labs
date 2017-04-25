module Logic(
  Value(..),
  Action(..),
  renderValue
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


mapAction :: (String -> String) -> Action -> Action
-- Change second argument inside of 'Action'.
mapAction f (Addition       x) = Addition       (f x)
mapAction f (Subtraction    x) = Subtraction    (f x)
mapAction f (Multiplication x) = Multiplication (f x)
mapAction f (Division       x) = Division       (f x)
mapAction f (SquareRoot     x) = SquareRoot     (f x)
mapAction f (Power          x) = Power          (f x)
mapAction f (Invert         x) = Invert         (f x)


renderValue :: Value -> String
-- Render given 'Value'.
renderValue (Value x action) =
  g x ++ f a ++ (if null y then "" else g y)
    where (a, y) = case action of
                      Nothing                   -> ("", "")
                      Just (Addition       arg) -> ("+", arg)
                      Just (Subtraction    arg) -> ("–", arg)
                      Just (Multiplication arg) -> ("*", arg)
                      Just (Division       arg) -> ("÷", arg)
                      Just (SquareRoot     arg) -> ("√", arg)
                      Just (Power          arg) -> ("^", arg)
                      Just (Invert         arg) -> ("±", arg)
          f "" = ""
          f l  = " " ++ l ++ " "
          g "" = "0"
          g xs = reverse xs


