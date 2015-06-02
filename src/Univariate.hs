module Univariate where

import Data.Set (Set)
import qualified Data.Set as S

data Subset = Subset (Set Properties) (Set Tests) Data
type Data = ()

type KSq = Double
type ChSq = Double
type DegF = Double
type PVal = Double
type SumSq = Double
type FVal = Double
type MeanSq = Double

data Tests = Bartlett KSq ChSq DegF PVal    -- Bartlett's Test
           | ANOVA DegF SumSq MeanSq FVal PVal
           deriving (Eq, Ord, Show)

data Properties = Homoscedastic     -- All vars have the same finite variance
                | Heteroscedastic   -- Sub-populations exist that have different variabilities from others.
                | SigVariance       -- Variance of the subsets is considered significant
                | InsigVariance     -- Variance of the subsets is considered insignificant
                deriving (Eq, Ord, Show)

-- | Determine homoscedasticity of a subset
-- TODO: Add requirement for gaussian distributed-ness
--       (http://www.r-bloggers.com/analysis-of-variance-anova-for-multiple-comparisons/)
bartlett :: Subset -> Subset
bartlett (Subset p ts xs)
    | pVal >= 0.05 && chSq > kSq = Subset (Homoscedastic   & p) (result & ts) xs
    | otherwise                  = Subset (Heteroscedastic & p) (result & ts) xs
    where result = Bartlett kSq chSq degF pVal
          kSq    = undefined
          chSq   = (undefined :: ChSq)
          pVal   = undefined
          degF   = undefined

-- | Determine if subset has sub-populations with significant variance
anova :: Subset -> Either String Subset
anova (Subset p ts xs)
    | Homoscedastic `S.notMember` p = Left ("Data is not proved to be homoscedastic!"
                                            ++ "Try running bartlett on the subset first.")
    | isSig xs  = Right $ Subset (SigVariance   & p) (result & ts) xs
    | otherwise = Right $ Subset (InsigVariance & p) (result & ts) xs
    where isSig = undefined
          result = undefined


{-- Utility Functions --}

(&) :: Ord a => a -> Set a -> Set a
x & xs = S.insert x xs

