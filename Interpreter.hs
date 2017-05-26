module Interpreter where
import Parser
import Operators
import Scanner

eval :: (Monad m) => Expr ->  m LoxObject
eval (Literal l) = return l
eval (Grouping g) = eval g
eval (Binary x op y) = do
  opfunc <- lookupBin op
  x' <- eval x
  y' <- eval y
  opfunc x' y'
eval (Unary op x) = do
  opfunc <- lookupUn op
  x' <- eval x
  opfunc x'
