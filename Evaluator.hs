module Evaluator where
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


main = do
  putStr "lox.hs>"
  input <- getLine
  result <- return $ (tokenize input :: Either String [Token]) >>= parse >>= eval
  case result of
    (Left failure) -> putStrLn $ "Error: " ++ failure
    (Right result') -> print result'
  main
