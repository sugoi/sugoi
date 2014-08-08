{-# LANGUAGE  FunctionalDependencies, FlexibleInstances, 
    GADTs, MultiParamTypeClasses, TemplateHaskell,
    TypeFamilies #-}

module Sugoi.Types where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Char
import Data.Time
import System.Locale
import System.Random

class FarmEnvironment env where
  type GeneOf env :: *
  type BenchmarkOf env :: *

type ResumeOf = Resume
type GeneHash = T.Text

data Resume env = Resume 
  { _gene :: GeneOf env 
  , _benchmarks :: [BenchmarkOf env]
  , _birthRecord :: T.Text}

makeClassy ''Resume


type Deck e m = m (ResumeOf e)
type Breeder e m = Deck e m -> m (ResumeOf e)

data Farm e m = Farm
  { _breeder :: Breeder e m
  , _deck :: Deck e m
  , _score :: BenchmarkOf e -> Double
  , _encoder :: GeneOf e -> T.Text
  , _decoders :: [T.Text -> Maybe (GeneOf e)]   
  , _measurement :: GeneOf e -> m (BenchmarkOf e)
  , _geneBank :: M.Map GeneHash (ResumeOf e)
  }



makeClassy ''Farm

  
randomGeneID :: IO T.Text
randomGeneID = do
  utct <- getCurrentTime
  tz <- getTimeZone utct
  let lt = utcToZonedTime tz utct
      ftstr = formatTime defaultTimeLocale "%z.%Y.%m%d.%H%M.%S" lt
      saltsrc = ['0'..'9']++['A'..'Z']++['a'..'z']
  salt <- 
    map (saltsrc!!) <$>
    replicateM 4 (randomRIO (0,length saltsrc - 1)) 
  return $ T.pack $ ftstr ++ salt
