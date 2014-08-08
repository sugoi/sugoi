{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Sugoi.Run where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map as M
import Data.Vector ((!))



import Sugoi.Types

mainLoop :: forall env m s. (m ~ MonadOf env, Monad m, MonadIO m, MonadState s m, HasFarm s env) 
  => m ()
mainLoop = do
  breeder0 <- use breeder
  deck0 <- use deck

  gene1 <- breeder0 deck0
  
  encoder0 <- use encoder
  let hash1 :: GeneHash
      hash1 = encoder0 (gene1 :: GeneOf env)
  msmt0 <- use measurement
  bench1 <- msmt0 gene1  
  let resume0 = Resume gene1 [bench1 :: BenchmarkOf env]
      
  bank0 <- use geneBank
  case M.lookup hash1 bank0 of
    _ -> return ()
  return ()
  
