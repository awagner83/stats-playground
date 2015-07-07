module Graphics.Chart
    ( Chart(..)
    , Settings(..)
    , Aesthetic(..)
    
    , drawSvg

    -- Settings Combinators
    , title
    , height
    , width

    -- Aesthetic Combinators
    , marker
    , fg
    , bg

    -- Layers
    , points
    , line
    ) where

import Graphics.Chart.Internal
import Graphics.Chart.Layers

