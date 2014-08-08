{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
module Sugoi.Combinator where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector ((!))
import System.Random


import Sugoi.Types

{-
mkBreeder1 :: ((Monad (MonadOf e))) => 
  ((ResumeOf e) -> (MonadOf e) (GeneOf e)) -> Breeder e
mkBreeder1 prog deck = do
  return undefined

deck0 ::  forall env m s. (m ~ MonadOf env, Monad m, MonadIO m, MonadState s m, HasFarm s env) 
  => Deck env
deck0 = do
  bank0 <- use geneBank
  let
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
  

  realIdx <- liftIO $ randomRIO (0, sumOfW)
  let i = maybe (nB-1) id $ V.findIndex (> realIdx) pileOfW 
  return $ bank0!i
  

            

-}