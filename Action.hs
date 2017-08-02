module Action where

import Data.List
import Control.Monad.Trans.Except
import BootstrapAction
import Object

type Action a = BootAction LoxObject a
type LoxEnvironment = Environment LoxObject

wrapEither :: Either String a -> Action a
wrapEither  = either throwE return

takeUntilM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
takeUntilM predicate [] = return []
takeUntilM predicate (x:xs) = do
  x' <- x
  if predicate x' then
    return [x']
  else do
    xs' <- takeUntilM predicate xs
    return $ x':xs'
