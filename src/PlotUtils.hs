-- |
-- Module      :  PlotUtils
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description  
--
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module PlotUtils where

-- Data
import Data.Maybe
import Data.List(sortBy)
import Data.Function(on)
import qualified Data.Set as Set

-- plotting
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Axis.Int
import Data.List(nub)
import Debug.Trace(traceShow)

addMapToLegend :: Plot x y -> [(String,Int)] -> Plot x y
addMapToLegend p _legendIndex =
    let legend  = p ^. plot_legend
        legendIndex = sortBy (compare `on` snd) $ nub _legendIndex
        renderPlotLegendLines p (Rect p1 p2) = 
          withLineStyle p $ do
            let y = (p_y p1 + p_y p2) / 2
            ps <- alignStrokePoints [Point (p_x p1) y, Point (p_x p2) y]
            strokePointPath ps
        d       = renderPlotLegendLines (defaultPlotLineStyle)
        !legend'= [("[" ++ show i ++ "] "++ x, d) | (x,i) <- legendIndex ] 
    in 
    traceShow (map fst legend, map fst legend') $ 
    p & plot_legend .~ legend'  


plotHistogram' :: FilePath -> [(String,Int)] -> IO()
plotHistogram' histogramFileName h = toFile def histogramFileName $ do
    layout_title .= "Merchant histogram"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst h)
    (p :: Plot PlotIndex Int) <- fmap plotBars $ bars ["occurences"] (addIndexes (map ((\x -> [x]) . snd) h)) 
    plot $ return  p 

plotHistogram :: FilePath -> [(String,Int)] -> IO()
plotHistogram histogramFileName h = toFile def histogramFileName $ do
    layout_title .= "Merchant histogram"
    layout_title_style . font_size .= 10
    let legendIndex = map fst h `zip` iterate (+1) 1 
    layout_x_axis . laxis_generate .= autoIndexAxis (map (show . snd) legendIndex)
    (p :: Plot PlotIndex Int) <- fmap plotBars $ bars ["occurences"] (addIndexes (map ((\x -> [x]) . snd) h)) 
    plot . return $ p `addMapToLegend` legendIndex
