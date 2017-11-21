module EnvUtils where
import BootstrapAction
import Action
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Object
import Statement
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Maybe
import Control.Monad

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

find' :: String -> LoxEnvironment -> Maybe LoxObject
find' n = Map.lookup n

find :: String -> Action LoxObject
find n = do
  st <- lift get
  let
    envs' = envs st
    (Tag scope') = scope st
  wrapEither $ --wrap
    maybe (Left $ "Variable '" ++ n ++ "' doesn't exist!") Right $
      join $ L.find isJust $ --find where lookup worked first
        map (find' n) $ -- try lookup in each
          map (flip lookup' envs') scope' -- get env list corresponding to scope

set' :: String -> LoxObject -> LoxEnvironment -> Maybe LoxEnvironment
set' n val env = if n `Map.member` env then Just (Map.insert n val env) else Nothing

set :: String -> LoxObject -> Action ()
set n val = do
  st <- lift get
  let
    envs' = envs st
    (Tag scope') = scope st
  envs''  <- wrapEither $ -- wrap maybe into monad
    maybe (Left $ "Variable '" ++ n ++ "' doesn't exist!") Right $
      join $ L.find isJust $ -- find first success
        map (\k -> modifyKey k (set' n val) envs') $ scope' --try from inner to outer scope to set it
  lift $ put $ st {envs = envs''}

declare :: String -> LoxObject -> Action ()
declare n val = do
  st <- lift get
  let
    envs' = envs st
    (Tag scope') = scope st
    envs'' = fromJust $ modifyKey (head scope') (Just . Map.insert n val) envs'
  lift $ put $ st {envs = envs''}

newEnv :: Action Int
newEnv = do
  st <- lift get
  let
    envs' = envs st
    net = firstNotIn (map fst envs') [1..] -- new environment tag
    envs'' = (net,Map.empty):envs'
  lift $ put $ st {envs = envs''}
  return net

newTag :: Action Tag
newTag = do
  st <- lift get
  net <- newEnv
  let
    (Tag scope') = scope st
    scope'' = Tag $ net:scope'
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
  newTag <- newTag
  withTag newTag callback

usedEnvs :: Action [Int]
usedEnvs = do
  st <- lift get
  let scp = fromTag $ scope st -- all envs in scope are used
      --we walk the envs in our scope to find values using envs that we need
      closures = concat $ map walk $ map snd (envs st) 
      walk = concat . map getClosures . Map.elems
      getClosures (Func _ (Tag ts)) = ts
      getClosures (Object t) = [t]
      getClosures _ = []

  return $ L.nub $ closures ++ scp
