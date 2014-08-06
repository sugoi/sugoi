{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies #-}
module Sugoi.BreederCombinator where

import Sugoi.Types


mkBreeder1 :: ((Monad (MonadOf e))) => 
  ((ResumeOf e) -> (MonadOf e) (GeneOf e)) -> Breeder e
mkBreeder1 prog deck = do
  return undefined

deck0 :: Deck env
deck0 = do
  realIdx <- liftIO $ randomRIO (0, sumOfW)
  let i = maybe (nB-1) id $ V.findIndex (> realIdx) pileOfW 
  return $ bank0!i
  

  where
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
            

