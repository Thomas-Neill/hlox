module Object where

data LoxObject = String {toString::String} | Number {toDouble::Double} | Boolean {toBool::Bool} | Nil deriving Eq

instance Show LoxObject where
  show (String s) = s
  show (Number n) = shownum n
  show (Boolean b) = show b
  show Nil = "nil"

commonStart :: String -> String -> Bool
commonStart "" x = True
commonStart x "" = True
commonStart (x:xs) (y:ys) = if x == y then commonStart xs ys else False

commonEnding :: String -> String -> Bool
commonEnding x y = commonStart (reverse x) (reverse y)

shownum :: Double -> String
shownum x =
  let
    str = show x
    isInt = commonEnding str ".0"
  in if isInt then init $ init str else str
