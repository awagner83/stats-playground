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
    , lineColor
    , fillColor
    , bgColor

    -- Layers
    , points
    , line

    -- Misc
    , scope
    , subset
    ) where

import Graphics.Chart.Internal
import Graphics.Chart.Layers

