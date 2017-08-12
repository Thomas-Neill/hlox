module Interpreter where

import BootstrapAction
import Parser
import Operators
import Action
import Object
import Statement
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad
import qualified Data.List as L

--utility functions
firstNotIn :: [Int] -> [Int] -> Int
firstNotIn ys (x:xs) = if x `elem` ys then firstNotIn ys xs else x

lookup' :: (Eq a) => a -> [(a,b)] -> b
lookup' a = fromJust . lookup a

replace :: (Eq k) => k -> v  -> [(k,v)] -> Maybe [(k,v)]
replace k v [] = Nothing
replace k v ((k',v'):kvs) = if k == k' then
  Just ((k',v):kvs)
    else
  fmap ((k',v'):) $ replace k v kvs

modifyKey :: (Eq k) => k -> (v -> Maybe v) -> [(k,v)] -> Maybe [(k,v)]
modifyKey x callback env = do
  val <- lookup x env
  val' <- callback val
  replace x val' env

purge :: (Eq k) => [k] -> [(k,v)] -> [(k,v)]
purge keys env = foldr (\(k,v) env' -> if k `elem` keys then (k,v):env' else env') [] env

find' :: LValue -> LoxEnvironment -> Maybe LoxObject
find' (Name n) = Map.lookup n

find :: LValue -> Action LoxObject
find lvalue = do
  st <- lift get
  let
    envs' = envs st
    (Tag scope') = scope st
  wrapEither $ --wrap
    maybe (Left $ "Variable '" ++ show lvalue ++ "' doesn't exist!") Right $
      join $ L.find isJust $ --find where lookup worked first
        map (find' lvalue) $ -- try lookup in each
          map (flip lookup' envs') scope' -- get env list corresponding to scope

set' :: LValue -> LoxObject -> LoxEnvironment -> Maybe LoxEnvironment
set' (Name n) val env = if n `Map.member` env then Just (Map.insert n val env) else Nothing

set :: LValue -> LoxObject -> Action ()
set lval val = do
  st <- lift get
  let
    envs' = envs st
    (Tag scope') = scope st
  envs''  <- wrapEither $ -- wrap maybe into monad
    maybe (Left $ "Variable '" ++ show lval ++ "' doesn't exist!") Right $
      join $ L.find isJust $ -- find first success
        map (\k -> modifyKey k (set' lval val) envs') $ scope' --try from inner to outer scope to set it
  lift $ put $ st {envs = envs''}

declare' :: LValue -> LoxObject -> LoxEnvironment -> LoxEnvironment
declare' (Name n) value env = Map.insert n value env

declare :: LValue -> LoxObject -> Action ()
declare lval val = do
  st <- lift get
  let
    envs' = envs st
    (Tag scope') = scope st
    envs'' = fromJust $ modifyKey (head scope') (Just . declare' lval val) envs'
  lift $ put $ st {envs = envs''}

newEnv :: Action Tag
newEnv = do
  st <- lift get
  let
    envs' = envs st
    (Tag scope') = scope st
    newTag = firstNotIn (map fst envs') [1..]
    scope'' = Tag $ newTag:scope'
    envs'' = (newTag,Map.empty):envs'
  lift $ put $ st {envs = envs''}
  return scope''

withTag :: Tag -> Action a -> Action a
withTag t callback = do
  st <- lift get
  let oldScope = scope st
  lift $ put $ st {scope = t}
  result <- callback
  st' <- lift get
  lift $ put $ st' {scope = oldScope}
  return result

withShadow :: Action a -> Action a
withShadow callback = do
  newTag <- newEnv
  withTag newTag callback

usedEnvs :: Action [Int]
usedEnvs = do
  st <- lift get
  let scp = fromTag $ scope st -- all envs in scope are used
      --we walk the envs in our scope to find values using envs that we need
      closures = concat $ map walk $ map (flip lookup' (envs st)) $ scp
      walk = concat . map getClosures . Map.elems
      getClosures (Func _ (Tag ts)) = ts
      getClosures _ = []

  return $ L.nub $ closures ++ scp

-- actual definitions
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

evalExpr (Variable v) = find v

evalExpr (Assignment l obj) = do
    obj' <- evalExpr obj
    set l obj'
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
      declare (Name argname) arg
      evalExpr body

evalExpr (Fun argnames body) = do
  closure <- fmap scope $ lift get
  return $ closureFuncWithArity closure (length argnames) $
    \args -> withTag closure $ withShadow $ do
      zipWithM_ declare (map Name argnames) args
      int <- fmap last $ takeUntilM significant $ map eval body
      case int of
        None -> return Nil
        (Result x) -> return x
        _ -> throwE "Unexpected interrupt in function."



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
  declare l val
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

gcSweep = do
  used <- usedEnvs
  st <- lift get
  let newenvs = purge used (envs st)
  liftIO $ print (envs st)
  liftIO $ print (newenvs)
  lift . put $ st {envs = newenvs}

initInterpreter :: Action ()
initInterpreter = do
  set' "input" input
  where
    set' str x = declare (Name str) x
    input = funcWithArity 0 (\[] -> fmap String $ liftIO getLine)
