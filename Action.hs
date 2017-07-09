module Action where

import qualified Data.Map.Strict as Map
import Data.List
import Object
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

data LoxEnvironment = Global (Map.Map String LoxObject) | Shadow LoxEnvironment (Map.Map String LoxObject)

--type Action = LoxEnvironment -> IO (Either String (LoxEnvironment))
type Action a = ExceptT String (StateT LoxEnvironment IO) a

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
