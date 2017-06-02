module Interpreter where

import Parser
import Operators
import Action
import Result

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

eval :: Statement -> Action
eval (Expression e) = \st -> evalExpr e st >>= (\wrapped' -> wrapped' ... (\(value,st')->return . Result $ st'))
eval (Print expr) = \st -> do
  wrapped <- evalExpr expr st
  case wrapped of
    (Failure f) -> putStrLn $ "Error: " ++ f
    (Result (value,_)) -> print value
  wrapped ... (\(_,st')-> return $ Result st')
