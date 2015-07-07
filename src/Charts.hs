{-# LANGUAGE PartialTypeSignatures, ViewPatterns, OverloadedStrings #-}
module Main where

import Data.Monoid
import Graphics.Chart
import Vga
import qualified Diagrams.Prelude as D

main = do
    push $ drawSvg points (sinePoints 50)
    push $ drawSvg line (sinePoints 70)
    push $ drawSvg (points <> line <> height 100) (sinePoints 100)
    push $ drawSvg line (tanPoints 100)
    push $ drawSvg (points <> line <> marker (D.square 5)) (logPoints 50)

sinePoints :: Int -> [(Double, Double)]
sinePoints n = take n $ zip [1,1.2..] $ map sin [1.0,1.2..]

tanPoints :: Int -> [(Double, Double)]
tanPoints n = take n $ zip [1,1.01..] $ map tan [1.0,1.01..]

logPoints :: Int -> [(Double, Double)]
logPoints n = take n $ zip [1,1.2..] $ map log [1.0,1.2..]

