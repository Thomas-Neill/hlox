module Action where

import qualified Data.Map.Strict as Map
import Data.List
import Object

data LoxEnvironment = Global (Map.Map String LoxObject) | Shadow LoxEnvironment (Map.Map String LoxObject)

type Action = LoxEnvironment -> IO (Either String (LoxEnvironment))
--I __should__ be using monad transformers, but I just can't figure the
--darned things out

compose :: Action -> Action -> Action
compose a b = \env -> do
  completed <- a env
  either (return . Left) b completed

composeList :: [Action] -> Action
composeList actions = foldl1' compose actions

type ReturnAction = LoxEnvironment -> IO (Either String (LoxObject,LoxEnvironment))

--Wrap a object into a ReturnAction
wrap :: LoxObject -> ReturnAction
wrap object = \st -> (return . return) (object,st)

wrapResult :: Either String LoxObject -> ReturnAction
wrapResult object = \st -> return $ either (Left) (\obj->Right (obj,st)) object

--this makes staircases of doom less horrible,
--it's basically a rewritten >>= that lifts into IO
(...) :: Either String a -> (a -> IO (Either String b)) -> IO (Either String b)
(...) resultValue f = case resultValue of
                            (Left err) -> return $ Left err
                            (Right value) -> f value
