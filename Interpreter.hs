module Interpreter where

import BootstrapAction
import Operators
import Action
import Object
import Statement
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import EnvUtils

findLV :: LValue -> Action LoxObject
findLV (Name n) = find n
findLV  (Access obj attr) = do
  obj' <- evalExpr obj
  st <- lift get
  let
    envs' = envs st
    attrs = lookup' (getEnv obj') envs'
    lked = Map.lookup attr attrs
  wrapEither $ maybe (Left $ "Object '" ++ show obj ++ "' doesn't have attribute '" ++ attr ++ "'") Right lked

setLV :: LValue -> LoxObject -> Action ()
setLV (Name n) v = set n v
setLV (Access obj attr) val = do
  obj' <- evalExpr obj
  st <- lift get
  let
    envs' = envs st
    oldAttrs = lookup' (getEnv obj') envs'
    (Just envs'') = replace (getEnv obj') (Map.insert attr val oldAttrs) envs'
  lift $ put st {envs = envs''}

declareLV :: LValue -> LoxObject -> Action ()
declareLV (Name n) = declare n

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

evalExpr (Variable v) = findLV v

evalExpr (Assignment l obj) = do
    obj' <- evalExpr obj
    setLV l obj'
    return obj'

evalExpr (InlineIf cond thn els) = do
  cond' <- evalExpr cond
  evalExpr $ if toBool $ truthiness cond' then thn else els

evalExpr (Funcall f args) = do
  func <- evalExpr f
  args <- mapM evalExpr args
  case func of
    (Func f _) -> f args
    _ -> throwE "Didn't get function for function call"

evalExpr (Rocket argname body) = do
  closure <- fmap scope $ lift get
  return $ closureFuncWithArity closure 1 $
    \[arg] -> withTag closure $ withShadow $ do
      declareLV (Name argname) arg
      evalExpr body

evalExpr (Fun argnames body) = do
  closure <- fmap scope $ lift get
  return $ closureFuncWithArity closure (length argnames) $
    \args -> withTag closure $ withShadow $ do
      zipWithM_ declareLV (map Name argnames) args
      int <- fmap last $ takeUntilM significant $ map eval body
      case int of
        None -> return Nil
        (Result x) -> return x
        _ -> throwE "Unexpected interrupt in function."

evalExpr (CreateObject nops) = do
  new <- newTag
  withTag new $ flip mapM_ nops $ \(name,val) -> do
    val' <- evalExpr val
    declare name val'
  return $ Object (head $ fromTag new)

data Interrupt = None | Stop | Result LoxObject deriving Eq

instance Monoid Interrupt where
  mappend None x = x
  mappend x None = x
  mappend x _ = x
  mempty = None

significant :: Interrupt -> Bool
significant = (None /=)

eval :: Statement -> Action Interrupt
eval (Expression e) = do
  evalExpr e
  return None

eval (Print e) = do
  result <- evalExpr e
  liftIO $ print result
  return None

eval (Declaration l e) = do
  val <- evalExpr e
  declareLV l val
  return None

eval (Compound stmts) = withShadow $ do
  ints <- takeUntilM significant $ map eval stmts
  return $ last ints

eval (If cond if' else') = do
  cond' <- evalExpr cond
  eval (if toBool $ truthiness cond' then if' else else')

eval (While cond body) = do
  cond' <- evalExpr cond
  if toBool $ truthiness cond' then do
    int <- eval body
    if significant int then
      if int == Stop then
        return None
      else
        return int
    else
      eval (While cond body)
  else return None

eval Empty = return None
eval Break = return Stop
eval (Return x) = evalExpr x >>= return . Result

tryGcSweep = do
  til <- lift $ gets untilNextSweep
  til' <-
    if til == 0 then do
      gcSweep
      return 5
    else if til == -1 then return (-1)
    else return (til - 1)
  lift $ modify $ \st -> st {untilNextSweep = til'}

gcSweep = do
  used <- usedEnvs
  st <- lift get
  let newenvs = purge used (envs st)
  lift . put $ st {envs = newenvs}

initInterpreter :: Action ()
initInterpreter = do
  set' "input" input
  set' "object" newObject
  set' "debugenvs" debugEnvs
  set' "scopeasobj" scopeAsObject
  set' "sweep" forceSweep
  set' "sweepon" yesSweep
  set' "sweepoff" noSweep
  where
    set' str x = declareLV (Name str) x
    input = funcWithArity 0 (\_ -> fmap String $ liftIO getLine)
    newObject = funcWithArity 0 (\_ -> fmap Object newEnv)
    debugEnvs = funcWithArity 0 (\_ -> lift (gets envs) >>= liftIO . print >> return Nil)
    scopeAsObject = funcWithArity 0 (\_ -> fmap (Object . head. fromTag) (lift $ gets scope))
    forceSweep = funcWithArity 0 (\_ -> gcSweep >> return Nil)
    noSweep = funcWithArity 0 (\_ -> lift $ modify (\st -> st {untilNextSweep = -1}) >> return Nil)
    yesSweep = funcWithArity 0 (\_ -> lift $ modify (\st -> st {untilNextSweep = 5}) >> return Nil)
