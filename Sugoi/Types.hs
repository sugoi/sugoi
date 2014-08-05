{-# LANGUAGE FunctionalDependencies, FlexibleInstances, GADTs, MultiParamTypeClasses,TemplateHaskell, TypeFamilies #-}
module Sugoi.SA123N where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V


type family GenomOf :: * -> *
type family MonadOf :: * -> (* -> *)
type family ScoreOf :: * -> *     


type Draw a = ((a -> Double) , Int)
type Deck m a = Draw a -> m [a]
type Breeder m a = Deck m a -> m a  

data Farm m a = Farm
  { _breeder :: Breeder m a
  , _encoder :: a -> T.Text
  , _decoders :: [T.Text -> Maybe a]   
  }
  
makeClassy ''Farm

data FarmState m a = FarmState
  { _database :: V.Vector a
  , _encodeDatabse :: V.Vector a -> m ()
  }

makeClassy ''FarmState