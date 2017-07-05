module Interpreter where

import Parser
import Operators
import Action
import Object
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.IO.Class

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

evalExpr :: Expr ->  Action LoxObject
evalExpr (Literal l) = return l
evalExpr (Grouping g) = evalExpr g
evalExpr (Binary x op y) =
  do
    op' <- wrapEither $ lookupBin op
    x' <- evalExpr x
    y' <- evalExpr y
    wrapEither $ op' x' y'

evalExpr (Unary op x) =
  do
    op' <- wrapEither $ lookupUn op
    x' <- evalExpr x
    wrapEither $ op' x'

evalExpr (Variable v) =
  do
    st <- lift get
    wrapEither $ find v st

evalExpr (Assignment l obj) =
  do
    obj' <- evalExpr obj
    st <- lift get
    st' <- (wrapEither $ set l obj' st)
    lift $ put st'
    return obj'

evalExpr (InlineIf cond thn els) = do
  cond' <- evalExpr cond
  evalExpr $ if toBool $ truthiness cond' then thn else els


eval :: Statement -> Action ()

eval (Expression e) = do
  evalExpr e
  return ()

eval (Print e) = do
  result <- evalExpr e
  liftIO $ print result

eval (Declaration l e) = do
  val <- evalExpr e
  st <- lift get
  lift $ put (declare l val st)

eval (Compound stmts) = do
  mapM_ eval stmts

eval (Empty) = return ()

eval (If cond if' else') = do
  cond' <- evalExpr cond
  eval (if toBool $ truthiness cond' then if' else else')

eval (While cond body) = do
  cond' <- evalExpr cond
  if toBool $ truthiness cond' then eval body >> eval (While cond body) else return ()
