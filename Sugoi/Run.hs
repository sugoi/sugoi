{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Sugoi.Run where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector ((!))

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import System.Random

import Sugoi.Types

mainLoop :: forall env m s. (m ~ MonadOf env, Monad m, MonadIO m, MonadState s m, HasFarm s env) 
  => m ()
mainLoop = do
  bank0 <- use geneBank
  breeder0 <- use breeder
  deck0 <- use deck

  newGene <- breeder0 deck0
  msmt <- use measurement
  bench <- msmt newGene  
  gid <- liftIO randomGeneID
  let newResume = Resume gid newGene [bench]
  geneBank %= (flip V.snoc newResume)
  
  return ()
  
