{-# LANGUAGE PartialTypeSignatures, ViewPatterns, OverloadedStrings #-}
module Charts where

import Data.List (genericLength)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Lucid.Base
import qualified Data.ByteString.Lazy as B

--main = mainWith $ frame 1 $
--              bars "Super important... chart review!" [50, 23, 100, 90]

--main = B.putStrLn $ renderBS example1

example1 :: _
example1 = renderDia SVG (SVGOptions (mkWidth 250) Nothing "") myDiagram
  where myDiagram = frame 1 rawDiagram
        rawDiagram = bars "Super important... chart review!" [50, 23, 100, 90]


-- | Bar-chart diagram
bars :: String -> [_] -> Diagram B
bars title xs = title'
                ===
                (hsep 0.1 . map (alignB . bar) . scale $ xs)
  where scale xs@(maximum -> m) = map (/m) xs
        title'      = (text title <> rect titleLength 1 # lw 0) # fontSize (local 1)
        titleLength = genericLength title
        n           = genericLength xs 

-- | Draw a single bar in a bar-chart
bar :: _ -> Diagram B
bar x = rect 0.5 x # fc grey # lc white

