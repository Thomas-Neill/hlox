module BootstrapAction where
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map

--we have to do this to prevent a circle dependency
type Environment a = Map.Map String a

--each int is like a pointer to a
--right side is inner, left is outer
newtype Tag = Tag [Int]
data LoxState a = LoxState {envs :: [(Int,Environment a)], scope :: Tag}

beginState = LoxState [(0,Map.empty)] (Tag [0])

type BootAction l a = ExceptT String (StateT (LoxState l) IO) a
