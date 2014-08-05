module SA123N.Execution where

import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.State

import SA123N.Types

mainLoop :: (Monad m, MonadIO m, MonadState s m, HasFarmState s m a) 
  => Farm m a -> m ()
mainLoop farm = do
  return ()
  
