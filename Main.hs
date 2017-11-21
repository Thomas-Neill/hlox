import Interpreter
import Parser
import BootstrapAction
import Action
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad
import System.Environment
import System.Exit

evalText :: String -> Action ()
evalText input = do
  stmts <- wrapEither $ either (Left . show) (Right) $ parsed "stdin" input
  ints <- mapM eval stmts
  unless (all (not . significant) ints) (throwE "Unexpected top-level interrupt")
  gcSweep
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

main = do
  contents <- fmap (\(Right x) -> x) $ parsedF "rc.lox"
  args <- getArgs
  case args of
    [] -> perform $ initInterpreter >> mapM eval contents >> loop
    [fname] -> do
      result <- parsedF fname
      case result of
        (Right contents2) -> perform $ do
          initInterpreter
          mapM (\x -> eval x >> tryGcSweep) contents
          mapM (\x -> (eval x >> tryGcSweep) `catchE` (\err -> liftIO $ die err)) contents2
        (Left err) -> print err
