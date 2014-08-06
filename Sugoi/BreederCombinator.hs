{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies #-}
module Sugoi.BreederCombinator where

import Sugoi.Types


mkBreeder1 :: ((Monad (MonadOf e))) => 
  ((ResumeOf e) -> (MonadOf e) (GeneOf e)) -> Breeder e
mkBreeder1 prog deck = do
  return undefined

