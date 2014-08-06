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
  let 
    deck0 :: Deck env
    deck0 = do
      realIdx <- liftIO $ randomRIO (0, sumOfW)
      let i = maybe (nB-1) id $ V.findIndex (> realIdx) pileOfW 
      return $ bank0!i
      
    weightFunction :: ResumeOf env -> Double
    weightFunction = error "xxx:weightFunction "
      
    wbank :: V.Vector (ResumeOf env, Double)
    wbank = V.map (\r -> (r, weightFunction r)) bank0

    nB :: Int
    nB = V.length bank0

    pileOfW :: V.Vector Double
    pileOfW = V.generate nB $ \i ->
      (snd $ wbank!i) + (if i == 0 then 0 else pileOfW ! (i-1))

    sumOfW :: Double
    sumOfW = V.last pileOfW 
            


  newGene <- breeder0 deck0
  msmt <- use measurement
  bench <- msmt newGene  
  gid <- liftIO randomGeneID
  let newResume = Resume gid newGene [bench]
  geneBank %= (flip V.snoc newResume)
  
  return ()
  
