-- |
-- Module      :  TimeSeriesPlot
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module TimeSeriesPlot where

-- Data
import Data.Maybe(fromMaybe)

-- Control
import Control.Monad(forM_)

-- plotting
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Axis.Int
import Control.Arrow(second)


data Side = LeftSide | RightSide
  deriving (Show, Eq, Read)

data Style = Lines | Points
  deriving (Show, Eq, Read)

data TimeSeries = TimeSeries{
                      series :: [(Side,String,[(Double,Double)])]
                    , filename :: String
                    , title :: Maybe String
                    , style :: Style
                  } deriving (Show, Read)

integrate ts = ts{ series = series ts ++ [i] }
  where (side, s, xs) = head $ series ts
        xs' = tail $ scanl (\(x,y) (x',y') -> (x', y+y')) (0,0) xs
        i   = (side, s ++ " integrated", xs')

plotTimeseries :: TimeSeries -> IO ()
plotTimeseries ts = toFile def plotFileName $ do
    layoutlr_title .= fromMaybe "time series plot" (title ts)
    forM_ (series ts) $ \(_s,_title,xys) -> do 
      let _plot | _s == LeftSide = plotLeft
                | otherwise  = plotRight 
          _line | style ts == Points = \s xs -> toPlot <$> points s xs
                | otherwise          = \s xs -> toPlot <$> line s [xs]
      _plot  (_line   _title    xys  )
  where plotFileName = filename ts
