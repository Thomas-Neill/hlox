module Interpreter where

import Parser
import Operators
import Action
import Object
import qualified Data.Map.Strict as Map

find :: LValue -> LoxEnvironment -> Either String LoxObject
find (Name n) (Global env) = case Map.lookup n env of
        Nothing -> Left $ "Variable '" ++ n ++ "' does not exist!"
        (Just obj) -> Right obj

find n (Shadow parent env) = case find n (Global env) of
        (Left _) -> find n parent
        (Right obj) -> Right obj

set :: LValue -> LoxObject -> LoxEnvironment -> Either String LoxEnvironment
set (Name n) replacement (Global env) = if Map.notMember n env
  then Left $ "Variable '" ++ n ++ "' does not exist!"
  else Right $ Global $ Map.insert n replacement env

set n replacement (Shadow parent env) = case set n replacement (Global env) of
        (Left _) -> case set n replacement parent of
                        (Left err) -> Left err
                        (Right parent') -> Right (Shadow parent' env)
        (Right asglobal) -> (\(Global env')-> Right (Shadow parent env')) asglobal

declare :: LValue -> LoxObject -> LoxEnvironment -> LoxEnvironment
declare (Name n) value (Global env) = Global $ Map.insert n value env

declare n value (Shadow parent env) =
  let
    env' = declare n value (Global env)
  in case env' of
    (Global env'') -> Shadow parent env''

--BEHOLD, THE STAIRCASE OF DOOM
--TODO: fix this somehow... it works, and "..." really helps, but this is still causing me pain
unwrap = flip (...)

evalExpr :: Expr ->  ReturnAction
evalExpr (Literal l) = wrap l
evalExpr (Grouping g) = evalExpr g
evalExpr (Binary x op y) = \st ->
  lookupBin op ... (\func -> evalExpr x st >>=
                        (\wrappedx -> wrappedx ... (\(x',st')->
                            evalExpr y st' >>= (\wrappedy -> wrappedy ...
                              (\(y',st'')-> func x' y' ...
                                (\value -> return $ Right (value,st'')
                    ))))))

evalExpr (Unary op x) = \st ->
  lookupUn op ... (\func -> evalExpr x st >>=
      (\wrappedx ->
          wrappedx ... (\(x',st')-> return $
            either (Left) (\value-> Right (value,st')) (func x'))))

evalExpr (Variable v) = \st -> find v st ... (\obj-> return . Right $ (obj,st))

evalExpr (Assignment l obj) = \st -> evalExpr obj st >>= (\wrapped ->
  wrapped ... (\(obj',st') ->
    set l obj' st ... (\st' ->
      return $ Right (obj',st')
    )))

evalExpr (InlineIf cond thn els) = \st ->
  evalExpr cond st >>= (unwrap (\(cond',st') ->
        if toBool $ truthiness cond' then evalExpr thn st' else evalExpr els st'
        ))

eval :: Statement -> Action
eval (Expression e) = \st -> evalExpr e st >>= (\wrapped' -> wrapped' ... (\(value,st')->return . Right $ st'))
eval (Print expr) = \st -> do
  wrapped <- evalExpr expr st
  case wrapped of
    (Left f) -> return ()
    (Right (value,_)) -> print value
  wrapped ... (\(_,st')-> return $ Right st')
eval (Declaration l expr) = \st -> do
  wrapped <- evalExpr expr st
  wrapped ... (\(value,st') -> return $ Right $ declare l value st')
eval (Compound stmts) = \st -> do
  let evaluated = map eval stmts
      glued = composeList evaluated
  wrapped <- glued (Shadow st Map.empty)
  wrapped ... (\(Shadow st' _)->return $ Right st')
eval (Empty) = \st -> return $ Right st
eval (If cond if' else') = \st -> do
  wrapped <- evalExpr cond st
  wrapped ... (\(cond',st') -> eval (if toBool $ truthiness cond' then if' else else') st)
eval (While cond body) = \st -> do
  wrapped <- evalExpr cond st
  wrapped ... (\(cond',st') -> if toBool $ truthiness cond' then compose (eval body) (eval (While cond body)) st else return $ Right st')
