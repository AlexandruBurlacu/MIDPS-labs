module Logic(
  Value(..),
  Action(..),
  backspace,
  clearEntry,
  clearAll,
  enterDigit,
  enterDot,
  operator,
  equals,
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


enterDot :: Value -> Value
-- Change state as if a dot is entered.
enterDot (Value x action) =
  let f xs = if '.' `elem` xs then xs else '.' : xs
    in case action of
       Nothing -> Value (f x) Nothing
       Just  a -> Value x (Just $ mapAction f a)


enterDigit :: Char -> Value -> Value
-- Change state as if specific char (digit) is entered.
enterDigit ch (Value x action) =
  case action of
    Nothing -> Value (ch:x) Nothing
    Just  a -> Value x (Just $ mapAction (ch:) a)


backspace :: Value -> Value
-- Change state as if last character of current argument is removed.
backspace (Value x action) =
  case action of
    Nothing -> Value (drop 1 x) Nothing
    Just  a -> Value x (Just $ mapAction (drop 1) a)


operator :: (String -> Action) -> Value -> Value
-- Apply given operator to current state. If some action is already fully
-- constructed, evaluate it first.
operator op value =
  let (Value x action) = equals value
  in Value x $ Just $
    case action of
      Nothing -> op ""
      Just  a -> op (getSndArg a)


clearEntry :: Value -> Value
-- Change state as if current argument is removed.
clearEntry (Value x action) =
  case action of
    Nothing -> Value "" Nothing
    Just  a ->
      if null (getSndArg a)
      then Value "" Nothing
      else Value x (Just $ mapAction (const "") a)


clearAll :: Value -> Value
-- Change state returning it to the default value.
clearAll = const (Value "" Nothing)


equals :: Value -> Value
-- Evaluate current calculator's state putting result in place of first
-- argument.
equals (Value x action) =
  case action of
    Nothing -> Value x Nothing
    Just  a ->
      if null (getSndArg a)
      then Value x action
      else Value result Nothing
        where
          g  :: String -> Double
          g ""       = 0
          g ('.':xs) = g ('0':'.':xs)
          g xs       = read (reverse xs)
          x' = g x
          y' = g (getSndArg a)

          result = reverse . show $
            case a of
              Addition       _ -> x' + y'
              Subtraction    _ -> x' - y'
              Multiplication _ -> x' * y'
              Division       _ -> x' / y'
              SquareRoot     _ -> sqrt x'
              Power          _ -> x' ** y'
              Invert         _ -> - x'


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
