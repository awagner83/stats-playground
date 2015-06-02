{-# LANGUAGE ViewPatterns #-}
module Regression where

data Line = Line Double Double deriving Show
data Calculation a b = Calc a [(String, b)] deriving Show

type Point = (Double, Double)

seL :: Line -> [Point] -> Double
seL (Line m b) ps = (nm (^2) ys)
                    - (2 * m * nm (mult) ps)
                    - (2 * b * nm id ys)
                    + (m^2 * nm (^2) xs)
                    + (2 * m * b * nm id xs)
                    + (n * b^2)
    where (xs, ys) = unzip ps
          n = fromIntegral . length $ ps

          mult (a, b) = a * b
          nm f = (n *) . mean . map f

-- | Total variation in y
seY :: [Point] -> Double
seY (map snd -> ys) = sum (map (\y -> (y - ym)^2) ys)
    where ym = mean ys

-- | Coefficient of determination (R^2)
--   Larger results represent a better fit (1 is a perfect fit)
r2 :: Line -> [Point] -> Double
r2 l ps = 1 - (seL l ps / seY ps)


-- | Average of a list of numbers
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

-- | Best fit determined by minimizing the squared-error of the line
bestLine :: [Point] -> Line
bestLine xys@(unzip -> (xs, ys)) = Line m b
    where m = (xm * ym - xym) / (xm^2 - x2m)
          b = ym - m * xm

          xm  = mean xs
          ym  = mean ys
          xym = mean $ map (uncurry (*)) xys
          x2m = mean $ map (^2) xs

calcLine :: [Point] -> Calculation Line Double
calcLine ps@(bestLine -> l) = Calc l [ ("SE (Line)", seL l ps)
                                     , ("SE (Y)"   , seY   ps)
                                     , ("R2"       , r2  l ps)
                                     ]


{-

Covariance between two random variables
=======================================

Cov(X, Y) = E[(X - E[X])(Y - E[Y])]
          = E[XY] - E[Y]E[X]

When we don't have the full population, we can approx these expected values:

E[XY] ~= sample mean XY
E[Y]  ~= sample mean X
E[Y]  ~= sample mean Y

or:

~Cov(X, Y) = (mean XY) - (mean y) *  (mean x)   -- This is also the numerator
                                                -- in the slope of the
                                                -- regression line

-----


Misc:

E[..] = Population Mean
E[E[X]] = E[X]
The covariance of a random variable with itself is really just the variance!

The slope of our regression line can also be thought of as Cov(X, Y) / Var(X)

-}



