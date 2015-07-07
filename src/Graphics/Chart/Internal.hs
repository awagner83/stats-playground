{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Graphics.Chart.Internal where

import Control.Applicative
import Data.Monoid
import Data.Optional
import Data.Colour
import Diagrams.Prelude (Diagram, renderDia, mkWidth)
import Diagrams.Backend.SVG
import Lucid.Base
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Diagrams.Prelude as D

data Chart a   = Chart Settings (Aesthetic) [(Chart a -> a -> Diagram B)]

data Settings  = Settings { title_  :: Optional String
                          , height_ :: Optional Double
                          , width_  :: Optional Double
                          } deriving Show
data Aesthetic = Aes      { marker_ :: Optional (Diagram B)
                          , fg_     :: Optional (Colour Double)
                          , bg_     :: Optional (Colour Double)
                          }

instance Show (Chart a) where
    show (Chart s _ xs) = unwords [ "Chart"
                                  , show s
                                  , (show $ length xs)
                                  , "layers"]

instance Monoid Settings where
    mempty = Settings Default Default Default
    mappend (Settings t1 h1 w1)
            (Settings t2 h2 w2) = Settings (t2 <|> t1) (h2 <|> h1) (w2 <|> w1)

instance Monoid Aesthetic where
    mempty = Aes Default Default Default
    mappend (Aes m1 f1 b1)
            (Aes m2 f2 b2) = Aes (m2 <|> m1) (f2 <|> f1) (b2 <|> b1)

instance Monoid (Chart a) where
    mempty = Chart mempty mempty []
    mappend (Chart s1 a1 xs1) (Chart s2 a2 xs2) = Chart s' a' xs'
        where s'  = s1  <> s2
              a'  = a1  <> a2
              xs' = xs1 <> xs2

-- | Render chart as bytestring (html and svg)
drawSvg :: Chart a -> a -> B.ByteString
drawSvg ((defaultChart <>) -> c@(Chart (Settings _
                                                 (Specific h)
                                                 (Specific w)) _ xs)) a = go
    where go = L.toStrict diagramSVG
          diagramSVG = renderBS $ renderDia SVG options framed
          options = SVGOptions (D.mkSizeSpec2D (Just w) (Just h)) Nothing ""
          framed = D.rect (w + 20) (h + 20) <> (D.center diagram)
          diagram = mconcat $ map (\f -> f c a) xs

-- | Default chart settings
defaultChart :: Chart a
defaultChart = height 250 <> width 500 <> marker (D.circle 2)

-- | Create a new chart with a single layer
layer :: (Chart a -> a -> Diagram B) -> Chart a
layer f = Chart mempty mempty [f]


{- Setting and Aesthetic combinators -}

emptySettings :: Settings
emptySettings = Settings Default Default Default

title :: String -> Chart a
title x  = Chart (emptySettings {title_    = Specific x}) mempty []

height, width :: Double -> Chart a
height x = Chart (emptySettings {height_   = Specific x}) mempty []
width  x = Chart (emptySettings {width_    = Specific x}) mempty []

emptyAes :: Aesthetic
emptyAes = Aes Default Default Default

marker :: Diagram B -> Chart a
marker x = Chart mempty (emptyAes {marker_ = Specific x}) []

fg, bg :: Colour Double -> Chart a
fg x     = Chart mempty (emptyAes {fg_     = Specific x}) []
bg x     = Chart mempty (emptyAes {bg_     = Specific x}) []

