module Interpreter where

import Parser
import Operators
import Action
import Result
import Object
import qualified Data.Map as Map

find :: LValue -> LoxEnvironment -> Result LoxObject
find (Name n) env = case Map.lookup n env of
        Nothing -> Failure $ "Variable '" ++ n ++ "' does not exist!"
        (Just obj) -> Result obj

set :: LValue -> LoxObject -> LoxEnvironment -> Result LoxEnvironment
set (Name n) replacement env = if Map.notMember n env
  then Failure $ "Variable '" ++ n ++ "' does not exist!"
  else Result $ Map.insert n replacement env

declare :: LValue -> LoxObject -> LoxEnvironment -> LoxEnvironment
declare (Name n) value env = Map.insert n value env


--BEHOLD, THE STAIRCASE OF DOOM
--TODO: fix this somehow... it works, and "..." really helps, but this is causing me pain
evalExpr :: Expr ->  ReturnAction
evalExpr (Literal l) = wrap l
evalExpr (Grouping g) = evalExpr g
evalExpr (Binary x op y) = \st ->
  lookupBin op ... (\func -> evalExpr x st >>=
                        (\wrappedx -> wrappedx ... (\(x',st')->
                            evalExpr y st' >>= (\wrappedy -> wrappedy ...
                              (\(y',st'')-> func x' y' ...
                                (\value -> return $ Result (value,st'')
                    ))))))

evalExpr (Unary op x) = \st ->
  lookupUn op ... (\func -> evalExpr x st >>=
      (\wrappedx ->
          wrappedx ... (\(x',st')-> return $
            result (Failure) (\value-> Result (value,st')) (func x'))))

evalExpr (Variable v) = \st -> find v st ... (\obj-> return . Result $ (obj,st))

evalExpr (Assignment l obj) = \st -> evalExpr obj st >>= (\wrapped ->
  wrapped ... (\(obj',st') ->
    set l obj' st ... (\st' ->
      return $ Result (obj',st')
    )))

eval :: Statement -> Action
eval (Expression e) = \st -> evalExpr e st >>= (\wrapped' -> wrapped' ... (\(value,st')->return . Result $ st'))
eval (Print expr) = \st -> do
  wrapped <- evalExpr expr st
  case wrapped of
    (Failure f) -> putStrLn $ "Error: " ++ f
    (Result (value,_)) -> print value
  wrapped ... (\(_,st')-> return $ Result st')
eval (Declaration l expr) = \st -> do
  wrapped <- evalExpr expr st
  case wrapped of
    (Failure f) -> return $ Failure f
    (Result (value,st')) -> return $ Result $ declare l value st'
