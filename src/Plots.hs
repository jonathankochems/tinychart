-- |
-- Module      :  Plots
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description  
--
module Plots where

import PlotUtils

-- Data
import Data.Maybe
import Data.List(sortBy)
import Data.Function(on)
import qualified Data.Set as Set


import Control.Monad(forM_)

-- plotting
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Axis.Int

data HistogramPlot = HistogramPlot {
    filters :: [(Bool, Int)] -- filters. (True, i) --> filter ((>=) i), (False, i) --> filter ((<=) i)
  , filename :: String
  , histogram :: [(Int,Int)]
  , legend    :: [(String, Int)]
  , deriveAbbreviateLegend    :: Bool
} deriving (Show, Read)

fromHistogram deriveLegend above _filters _filename _histogram 
  = HistogramPlot{ filters   = map (\x -> (above, x)) _filters
                 , filename  = _filename
                 , histogram = [(1,y) | (x,y) <- _histogram ]
                 , legend    = [(x,1) | (x,y) <- _histogram ]
                 , deriveAbbreviateLegend = deriveLegend 
                 }

histogramPlots :: HistogramPlot -> IO ()
histogramPlots hplot = 
      forM_ fs $ \(b,f) -> do
        let op | not b     = (>=)
               | otherwise = (<=) 
            updown | b         = "above"
                   | otherwise = "below"
        _plot (file++updown++show f ++".svg") $ filter (op f . snd) _histogram
        print . sum . map snd $ filter ( op f . snd) _histogram
   where _histogram = [(x,y) | ((_,y),(x,_)) <- zip (histogram hplot) $ legend hplot]
         fs        = filters hplot
         file      = filename hplot
         _plot | deriveAbbreviateLegend hplot = plotHistogram
               | otherwise                    = plotHistogram'
