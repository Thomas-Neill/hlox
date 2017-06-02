import Interpreter
import Parser
import Action
import Scanner
import Result
import qualified Data.Map as Map

evalText :: String -> IO (Result (LoxEnvironment))
evalText input = let
  actions = (return input >>= tokenize >>= parse) :: Result [Statement]
  in case actions of
    (Failure errmsg) -> do
      putStrLn $ "Error: " ++ errmsg
      return $ Failure errmsg
    (Result statements) -> composeList (map eval statements) $ Map.fromList []
main = do
  putStr "lox.hs> "
  input <- getLine
  if input /= "quit"
  then do
      evalText input
      main
  else return ()
