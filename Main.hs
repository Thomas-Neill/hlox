import Interpreter
import Parser
import Scanner
import Result

evalText :: String -> String
evalText input = show $ (tokenize input :: Result [Token]) >>= parse >>= eval

main = do
  putStr "lox.hs> "
  result <- fmap evalText getLine
  putStrLn result
  main
