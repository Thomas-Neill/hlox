import Interpreter
import Parser
import Action
import Scanner
import Result
import qualified Data.Map as Map

evalText :: String -> LoxEnvironment -> (IO (Result LoxEnvironment,Bool))
evalText input env = let
  actions = (return input >>= tokenize >>= parse) :: Result [Statement]
  in case actions of
    (Failure errmsg) -> return (Failure errmsg,False)
    (Result statements) -> fmap (\res->(res,True)) $ composeList (map eval statements) env

loop st = do
  putStr "lox.hs> "
  input <- getLine
  if input /= "quit"
  then do
      newst <- evalText input st
      case newst of
        (Failure errmsg,True) -> do
          putStrLn $ "Runtime error: " ++ errmsg
          loop st
        (Failure errmsg,False) -> do
          putStrLn $ "Syntax/parser error: " ++ errmsg
          loop st
        (Result st',True) -> loop st'
  else return ()

main = loop (Global Map.empty)
