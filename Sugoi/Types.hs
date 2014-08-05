{-# LANGUAGE FunctionalDependencies, FlexibleInstances, GADTs, MultiParamTypeClasses,TemplateHaskell, TypeFamilies #-}
module Sugoi.Types where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V


class FarmEnvironment env where
  type GenomOf env :: *
  type MonadOf env :: * -> *
  type BenchmarkOf env :: *


type ResumeOf env = (GenomOf env, [BenchmarkOf env])




type Draw e = ((ResumeOf e -> Double) , Int) 
type Deck e = Draw e -> (MonadOf e) [ResumeOf e]
type Breeder e = Deck e -> (MonadOf e) (GenomOf e)

data Farm e = Farm
  { _breeder :: Breeder e
  , _encoder :: ResumeOf e -> T.Text
  , _decoders :: [T.Text -> Maybe (ResumeOf e)]   
  , _measurement :: GenomOf e -> (MonadOf e) (BenchmarkOf e)
  , _genomBank :: V.Vector (ResumeOf e)
  }

makeClassy ''Farm

  
