module Object where

import BootstrapAction
import Control.Monad.Trans.Except

data LoxObject = String {toString::String}
  | Number {toDouble::Double}
  | Boolean {toBool::Bool}
  | Func {call :: [LoxObject] -> BootAction LoxObject LoxObject,closure :: Tag}
  | Nil

closureFuncWithArity cls n f = flip Func cls $ \l ->
                      if length l /= n then
                        throwE $ "Expected " ++ show n ++ " arguments, got " ++ show (length l)
                      else f l

funcWithArity = closureFuncWithArity (Tag [])

instance Eq LoxObject where
  (String s) == (String s') = s == s'
  (Number n) == (Number n') = n == n'
  (Boolean b) == (Boolean b') = b == b'
  Nil == Nil = True
  _ == _ = False

instance Show LoxObject where
  show (String s) = s
  show (Number n) = shownum n
  show (Boolean b) = show b
  show (Func _ _) = "[Function]"
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
