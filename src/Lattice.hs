-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- Lattice.hs
-- lattice model to implement value process 

module Lattice (
    disc,
    absorb,
    exch,
    expectedValue
)
where

    
import Numeric

import ValueProcess
import Environment

-- Disc primitive
disc :: Currency -> (PR Bool, PR Double) -> PR Double
disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)

discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
    if and bRv -- test for horizon
        then [pRv]
        else
            let
                rest@(nextSlice:_) = discCalc bs ps rs
                discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                thisSlice = zipWith3 (\b p q -> if b then p else q) bRv pRv discSlice
            in thisSlice : rest

prevSlice :: RV Double -> RV Double
prevSlice [] = []
prevSlice (_:[]) = []
prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest


-- Absorb primitive
absorb :: Currency -> (PR Bool, PR Double) -> PR Double
absorb k (PR bSlices, PR rvs) =
            PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                        bSlices rvs


-- Exchange rate model
exch :: Currency -> Currency -> PR Double
exch k1 k2 = PR (konstSlices 1)


-- Expected value
expectedValue :: RV Double -> RV Double -> Double
expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

expectedValuePr :: PR Double -> [Double]
expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice


-- Probability calculation
probabilityLattice :: [RV Double]
probabilityLattice = probabilities pathCounts
    where
        probabilities :: [RV Integer] -> [RV Double]
        probabilities (sl:sls) = map (\n -> (fromInteger n) / (fromInteger (sum sl))) sl : probabilities sls

        pathCounts :: [RV Integer]
        pathCounts = paths [1] where paths sl = sl : (paths (zipWith (+) (sl++[0]) (0:sl)))