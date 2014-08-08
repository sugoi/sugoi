{-# LANGUAGE  FunctionalDependencies, FlexibleInstances, 
    KindSignatures,
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


type ResumeOf = Resume
type GeneHash = T.Text

class FarmTypeMembers (m :: * -> *) where
  type GeneOf m :: *
  type BenchmarkOf m :: *


data Resume m = Resume 
  { _gene :: GeneOf m
  , _benchmarks :: [BenchmarkOf m]
  , _birthRecord :: FamilyTree}

data FamilyTree 
 = FromDatabase GeneHash
 | DrawnBy T.Text FamilyTree
 | BredBy T.Text [FamilyTree]
 

makeClassy ''Resume


type Deck m = m (ResumeOf m)
type Breeder m = Deck m -> m (ResumeOf m)


data Farm m = Farm
  { _breeder :: Breeder m
  , _deck :: Deck m
  , _score :: BenchmarkOf m -> Double
  , _encoder :: GeneOf m -> T.Text
  , _decoders :: [T.Text -> Maybe (GeneOf m)]   
  , _measurement :: GeneOf m -> m (BenchmarkOf m)
  , _geneBank :: M.Map GeneHash (ResumeOf m)
  }
  

makeClassy ''Farm

scoreMeanDevi :: HasFarm t m => Getter t ([BenchmarkOf m] -> (Double, Double)) 
scoreMeanDevi = to (\farm0 -> meanDevi . map (farm0 ^.score) )
  where
    meanDevi :: [Double] -> (Double, Double)
    meanDevi xs = (meanX,deviX)
      where n = fromIntegral $ length xs
            meanX = if n <=0 then 0 else sum xs/n
            deviX = if n <=1 then 1 else sqrt $ sum[(x-meanX)^2| x<-xs] / (n-1)

scoreDevi :: HasFarm t m => Getter t ([BenchmarkOf m] -> Double)
scoreDevi = to (\farm0 -> snd . (farm0 ^.scoreMeanDevi) )

scoreMean :: HasFarm t m => Getter t ([BenchmarkOf m] -> Double) 
scoreMean = undefined

  
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
