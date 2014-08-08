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
  , _birthRecord :: FamilyTree}

data FamilyTree 
 = FromDatabase GeneHash
 | DrawnBy T.Text FamilyTree
 | BredBy T.Text [FamilyTree]
 

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

scoreMeanDevi :: Getter (Farm e m) ([BenchmarkOf e] -> (Double,Double))
scoreMeanDevi = to (\farm0 -> meanDevi . map (farm0 ^.score) )
  where
    meanDevi :: [Double] -> (Double, Double)
    meanDevi xs = (meanX,deviX)
      where n = fromIntegral $ length xs
            meanX = if n <=0 then 0 else sum xs/n
            deviX = if n <=1 then 1 else sqrt $ sum[(x-meanX)^2| x<-xs] / (n-1)

scoreMean :: Getter (Farm e m) ([BenchmarkOf e] -> Double)
scoreMean = to (\farm0 -> fst . (farm0 ^.scoreMeanDevi) )

scoreDevi :: Getter (Farm e m) ([BenchmarkOf e] -> Double)
scoreDevi = to (\farm0 -> snd . (farm0 ^.scoreMeanDevi) )




  
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
