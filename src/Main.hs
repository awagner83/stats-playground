-- | Main entry point to the application.
module Main where

import Regression
import Charts

points1 = [(1, 2), (2, 1), (4, 3)]
points2 = [(-2, -3), (-1, -1), (1, 2), (4, 3)]

main :: IO ()
main = do
    print $ calcLine points1
    print $ calcLine points2

