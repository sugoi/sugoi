{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Sugoi.Run where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Digest.Pure.MD5 as MD5
import Data.Vector ((!))



import Sugoi.Types

mainLoop :: forall e m s. (Monad m, MonadIO m, MonadState s m, HasFarm s m) 
  => m ()
mainLoop = do
  breeder0 <- use breeder
  deck0 <- use deck

  resume1 <- breeder0 deck0
  
  let gene1 :: GeneOf m
      gene1 = resume1 ^. gene

  hash1 <- hashGene gene1 
  msmt0 <- use measurement
  bench1 <- msmt0 gene1  
      
  bank0 <- use geneBank
  case M.lookup hash1 bank0 of
    _ -> return ()
  return ()
  



hashGene :: forall m s. (Monad m, MonadState s m, HasFarm s m) 
  => GeneOf m -> m GeneHash
hashGene gene1 = do
  encoder0 <- use encoder
  return $ T.pack $ show $ MD5.md5 $ BS.pack $ T.unpack $ encoder0 gene1                   