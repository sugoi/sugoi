module Sugoi.Run where

import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.State

import Sugoi.Types

mainLoop :: (Monad m, MonadIO m, MonadState s m, HasFarmState s m a) 
  => Farm m a -> m ()
mainLoop farm = do
  return ()
  
