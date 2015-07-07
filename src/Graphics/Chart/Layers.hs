{-# LANGUAGE FlexibleContexts, GADTs #-}
module Graphics.Chart.Layers where

import Data.Optional
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Chart.Internal (Chart(..), Settings(..), Aesthetic(..), layer)
import qualified Graphics.Chart.Internal as G

-- | Plot points
points :: Chart [(Double, Double)] B
points = layer go where
    go (Chart (Settings { G.height_ = Specific h
                        , G.width_  = Specific w
                        })
              (Aes      { G.marker_ = Specific m })
              _
       ) xs = let xs' = zip x' y'
                  (x , y ) = unzip xs
                  (x', y') = (scaleVals w x, scaleVals h y)
                  p (x, y) = m # translate (x ^& y) # fc black
              in mconcat (map p xs')
    go _ _ = error "points: Empty Settings or Aesthetic values!"

-- | Plot points as line
line :: Chart [(Double, Double)] B
line = layer go where
    go (Chart (Settings { G.height_ = Specific h
                        , G.width_  = Specific w
                        })
              (Aes      { G.marker_ = Specific m })
              _
       ) xs = let xs' = zip x' y'
                  (x , y ) = unzip xs
                  (x', y') = (scaleVals w x, scaleVals h y)
              in fromVertices (map p2 xs')
    go _ _ = error "line: Empty Settings or Aesthetic values!"

-- | Scale points to some fixed interval [0..n]
scaleVals :: (Ord n, Fractional n) => n -> [n] -> [n]
scaleVals s xs = map go xs where
    go x = ((x - minX) / (maxX - minX)) * s
    minX = minimum xs
    maxX = maximum xs

