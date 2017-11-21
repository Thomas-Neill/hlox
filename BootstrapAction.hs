module BootstrapAction where
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map

--we have to do this to prevent a circle dependency

type Environment a = Map.Map String a

--each int is like a pointer to a env
--right side is inner, left is outer
newtype Tag = Tag {fromTag :: [Int]} deriving Show
data LoxState a = LoxState {envs :: [(Int,Environment a)],
                            scope :: Tag,
                            untilNextSweep :: Int} deriving Show

beginState = LoxState [(0,Map.empty)] (Tag [0]) 5 -- 5 statements

type BootAction l a = ExceptT String (StateT (LoxState l) IO) a

--Why not just have Haskell refcount for me by IORef?
--That is a good question.
--Mainly, it's to make sure I control the semantics of
--refcounting rather than the language. (and it's too late to do anything else now)
