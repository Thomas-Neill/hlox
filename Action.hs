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

compose :: Action a -> Action a -> Action a
compose  = (>>)

composeList :: [Action a] -> Action a
composeList actions = foldl1' compose actions

--type ReturnAction = LoxEnvironment -> IO (Either String (LoxObject,LoxEnvironment))

--Wrap a object into a ReturnAction

--this makes staircases of doom less horrible,
--it's basically a rewritten >>= that lifts into IO
(...) :: Either String a -> (a -> IO (Either String b)) -> IO (Either String b)
(...) resultValue f = case resultValue of
                            (Left err) -> return $ Left err
                            (Right value) -> f value
