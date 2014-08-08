{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
module Sugoi.Combinator where

import Control.Applicative ((<|>), (<$>))
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

thermalDeck ::  forall m s. (Monad m, MonadIO m, MonadState s m, HasFarm s m) 
  => Deck m
thermalDeck = do
  bank0 <- use geneBank
  score0 <- use scoreMean 
  scoreDevi0 <- use scoreDevi
  let
      bankVector :: V.Vector (GeneHash, ResumeOf m)  
      bankVector = V.fromList $ M.toList bank0

      scoreOfResume :: ResumeOf m -> Double
      scoreOfResume = score0  . _benchmarks 

      nB :: Int
      nB = V.length bankVector

      maxScore :: Double
      maxScore = V.maximum $ V.map (scoreOfResume . snd) $ bankVector
   
  logTemperature0 <- liftIO $ randomRIO (0,log maxScore)


  let
      temperature0 :: Double
      temperature0 = exp logTemperature0
      
      weightFunction :: ResumeOf m -> Double
      weightFunction x =  exp((scoreOfResume x - maxScore)/temperature0)
        
      weightBank :: V.Vector (ResumeOf m, Double)
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



mkBreeder1 :: forall m s. (Monad m, MonadIO m, MonadState s m, HasFarm s m, GeneOf m ~ V.Vector Int) 
  => Breeder m
mkBreeder1 deck = do
  parentResume1 <- deck
  let gene1 :: V.Vector Int
      gene1 = parentResume1 ^. gene
  
      nG :: Double
      nG = fromIntegral $ V.length gene1

      xA = V.minimum gene1
      xB = V.maximum gene1
      
      nAmp :: Double
      nAmp = fromIntegral $ xB-xA

  frequency <- liftM exp $ liftIO $ randomRIO (0,log nG)

  amplitude <- liftM exp $ liftIO $ randomRIO (0,log nAmp)

  gene2 <- V.forM gene1 $ \val -> do
    p <- liftIO $ randomRIO (0,1)
    if p > frequency 
       then return val
       else liftIO $ perturbI nAmp val
  return parentResume1{_gene = gene2}


perturbI :: Double -> Int -> IO Int
perturbI amp val = do
  diffR <- randomRIO (-amp,amp)
  let diffBody = floor diffR
      diffCarryR = diffR - fromIntegral diffBody
  diffCarryP <- randomRIO (0,1)
  let diffCarry = if diffCarryP > diffCarryR then 0 else 1
  return $ val + diffBody + diffCarry
