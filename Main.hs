import Interpreter
import Parser
import Action
import Scanner
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad

evalText :: String -> Action ()
evalText input = do
  stmts <- (wrapEither $ tokenize input >>= parse)
  ints <- mapM eval stmts
  unless (all (not . significant) ints) (throwE "Unexpected top-level interrupt")
  return ()

loop :: Action ()
loop = do
  liftIO $ putStr "lox.hs> "
  input <- liftIO getLine
  oldst <- lift get
  if input /= "quit"
  then do
      evalText input `catchE` (\err -> do
        liftIO $ putStrLn err
        lift $ put oldst)
      loop
  else return ()

main = runStateT (runExceptT  loop) (Global Map.empty)
