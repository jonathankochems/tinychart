-- |
-- Module      :  CrossPlots
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module CrossPlots where

-- Data
import Data.Maybe
import Data.List(sortBy, nub)
import Data.Function(on)
import qualified Data.Set as Set

-- Control
import Control.Monad(when, unless)

-- plotting
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Axis.Int

import PlotUtils

data CrossPlot = CrossPlot {
    dataPoints :: [(Int,String)]
  , filename :: String
  , surpressLegend :: Bool
} deriving (Show, Read)

crossPlot :: CrossPlot -> IO ()
crossPlot crossplot = toFile def plotFileName $ do
    let stringYs    = Set.fromList $ map snd xys
        x0          = minimum $ map fst xys
        xmax        = maximum $ map fst xys
        xys'        = [(x, y `Set.findIndex` stringYs) | (x,y) <- xys]         
        legendIndex = sortBy (compare `on` snd) $ nub [(y, y `Set.findIndex` stringYs) | (x,y) <- xys] 
    layout_title .= "CrossPlot"
    liftCState $ shapes %= (:) PointShapeCross
    plot $  
      liftEC $ do
        color <- takeColor
        shape <- takeShape
        plot_points_values .= xys'
        plot_points_title .= "data"
        plot_points_style . point_color .= color
        plot_points_style . point_shape .= shape
        plot_points_style . point_radius .= 10
   
        let isFilled :: PointShape -> Bool
            isFilled PointShapeCircle = True
            isFilled PointShapePolygon{} = True
            isFilled _ = False 

        -- Show borders for unfilled shapes
        unless (isFilled shape) $ do
            plot_points_style . point_border_color .= color
            plot_points_style . point_border_width .= 1
    plot $ do setColors [opaque white]
              points "" [(x0,-1),(xmax+1,Set.size stringYs)]

    when (not $ surpressLegend crossplot) $
      layout_plots %= \ps ->
        let p = head ps
        in
        p `addMapToLegend` legendIndex : tail ps
  where plotFileName = filename crossplot
        xys          = dataPoints crossplot
