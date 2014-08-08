{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
module Sugoi.Combinator where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector ((!))
import System.Random
import Text.Printf

import Sugoi.Types

thermalDeck ::  forall e m s. (Monad m, MonadIO m, MonadState s m, HasFarm s e m) 
  => Double -> Deck e m
thermalDeck temperature0 = do
  bank0 <- use geneBank
  score0 <- use scoreMean 
  let
      bankVector :: V.Vector (GeneHash, ResumeOf e)  
      bankVector = V.fromList $ M.toList bank0

      scoreOfResume :: ResumeOf e -> Double
      scoreOfResume = score0  . _benchmarks 

      nB :: Int
      nB = V.length bankVector

      maxScore :: Double
      maxScore = V.maximum $ V.map (scoreOfResume . snd) $ bankVector

      weightFunction :: ResumeOf e -> Double
      weightFunction x =  exp((scoreOfResume x - maxScore)/temperature0)
        
      weightBank :: V.Vector (ResumeOf e, Double)
      weightBank = V.map (\(_,r) -> (r, weightFunction r)) bankVector
      
      pileOfW :: V.Vector Double
      pileOfW = V.generate nB $ \i ->
        (snd $ weightBank!i) + (if i == 0 then 0 else pileOfW ! (i-1))
      
      sumOfW :: Double
      sumOfW = V.last pileOfW 
  

  realIdx <- liftIO $ randomRIO (0, sumOfW)
  let i = maybe (nB-1) id $ V.findIndex (> realIdx) pileOfW 
      (hash1,ret1) = bankVector ! i
      logStr = T.pack $ printf "Temperature %f"                  
  return $ ret1 {_birthRecord = DrawnBy logStr $ FromDatabase hash1}
  





{-
mkBreeder1 :: ((Monad (MonadOf e))) => 
  ((ResumeOf e) -> (MonadOf e) (GeneOf e)) -> Breeder e
mkBreeder1 prog deck = do
  return undefined

            

-}