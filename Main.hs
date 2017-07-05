import Interpreter
import Parser
import Action
import Scanner
import qualified Data.Map as Map

evalText :: String -> LoxEnvironment -> (IO (Either String LoxEnvironment,Bool))
evalText input env = let
  actions = return input >>= tokenize >>= parse
  in case actions of
    (Left errmsg) -> return (Left errmsg,False)
    (Right statements) -> fmap (\res->(res,True)) $ composeList (map eval statements) env

loop st = do
  putStr "lox.hs> "
  input <- getLine
  if input /= "quit"
  then do
      newst <- evalText input st
      case newst of
        (Left errmsg,True) -> do
          putStrLn $ "Runtime error: " ++ errmsg
          loop st
        (Left errmsg,False) -> do
          putStrLn $ "Syntax/parser error: " ++ errmsg
          loop st
        (Right st',True) -> loop st'
  else return ()

main = loop (Global Map.empty)
