module Action where

import Parser (LoxObject)

import Result

import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Map.Strict as Map

type LoxEnv = Map.Map String LoxObject

newtype ResultT m a = ResultT {
  runResultT :: m (Result a)
}

instance (Functor f) => Functor (ResultT f) where
  fmap f = ResultT . fmap (fmap f) . runResultT

instance (Applicative a) => Applicative (ResultT a) where
  pure = ResultT . pure . Result
  f <*> a = ResultT $ liftA2 (<*>) (runResultT f) (runResultT a)

instance (Monad m) => Monad (ResultT m) where
  return = pure
  x >>= f = ResultT $ do
    v <- runResultT x
    result (return . Failure) (runResultT . f) v
  fail = ResultT . return . Failure

instance MonadTrans ResultT where
  lift = ResultT . fmap (Result)

liftResult = ResultT . return

instance (MonadIO m) => MonadIO (ResultT m) where
  liftIO = lift . liftIO

{-} .. test ..
type Action a = ResultT (StateT (Map.Map String String) IO) a

doStuff :: Action ()
doStuff = do
  liftIO $ putStr "Test> "
  input <- liftIO $ getLine
  let (op:args) = words input
  case op of
    "put" -> (lift . modify) (Map.insert (args !! 0) (args !! 1))
    "get" -> do
      state <- lift get
      value <- liftResult . toResult $ Map.lookup (args !! 0) state
      liftIO $ putStrLn value
    _ -> fail "Invalid input :("

main = do
  (runStateT $ runResultT $ (forever doStuff)) $ Map.fromList []
  return ()
{-}
