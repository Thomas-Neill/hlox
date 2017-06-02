module Action where

import qualified Data.Map as Map
import Object
import Result

type LoxEnvironment = Map.Map String LoxObject

type Action = LoxEnvironment -> IO (Result (LoxEnvironment))
--I __should__ be using monad transformers, but I just can't figure the
--darned things out

compose :: Action -> Action -> Action
compose a b = \env -> do
  completed <- a env
  result (return . Failure) b completed

composeList :: [Action] -> Action
composeList actions = foldl compose (head actions) (tail actions)

type ReturnAction = LoxEnvironment -> IO (Result (LoxObject,LoxEnvironment))

--Wrap a object into a ReturnAction
wrap :: LoxObject -> ReturnAction
wrap object = \st -> (return . return) (object,st)

wrapResult :: Result (LoxObject) -> ReturnAction
wrapResult object = \st -> return $ result (Failure) (\obj->Result (obj,st)) object

--this makes staircases of doom less horrible,
--it's basically a rewritten >>=
(...) :: Result a -> (a -> IO (Result b)) -> IO (Result b)
(...) resultValue f = case resultValue of
                            (Failure err) -> return $ Failure err
                            (Result value) -> f value
