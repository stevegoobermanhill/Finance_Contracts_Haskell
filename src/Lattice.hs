{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Lattice 
where

import PDist
import Data.Map.Strict

-- class LatticeElement is a traversable element
-- supporting next
class LatticeElement a where
    next :: a -> a -- next element     

-- LatticeDate is a generalization of the idea of a date
-- doesn't have to be a date (could start with an int!)
class (Ord a, LatticeElement a) => LatticeDate a where
    prev :: a -> a -- prev date


-- LatticeValueModel combines an array of  LatticeDate and LatticeDist together
-- with a representative symbol (currency, equity etc)
data LatticeSlice d v = forall d v. (LatticeDate d, LatticeElement v) => LatticeSlice{date :: d, slice :: v}
instance LatticeElement (LatticeSlice d ld) where
    next LatticeSlice{date = d,slice = sl} = LatticeSlice{date = next d, slice = next sl}

type Lattice d pd = [LatticeSlice d pd]

makeLattice :: LatticeSlice d ld -> Lattice d ld
makeLattice ls = ls : makeLattice (next ls)

data LatticeValueModel s d v = forall d v. (LatticeDate d, LatticeElement v) => LatticeValueModel{symbol :: s, model :: Lattice d v }

-- IntDate is an instantiation of date through an integer avlue (time period number)
newtype IntDate = IntDate{unIntDate :: Int}
instance Eq IntDate where
    (==) x y = (unIntDate x) == (unIntDate y)
instance Ord IntDate where
    (<=) x y = (unIntDate x) <= (unIntDate y)
instance LatticeElement IntDate where
    next i = IntDate $ (unIntDate i) + 1
instance LatticeDate IntDate where
    prev i = IntDate $ (unIntDate i) - 1



--SimpleRateElement is a LatticeSlice where elements transform in a multiplicative fashion
upVol :: Double -> Double -> Double
upVol volatility value = volatility * value

downVol :: Double -> Double -> Double
downVol volatility value = volatility / value

binaryStep :: Double -> PDist (Double -> Double)
binaryStep vol = normalize $ PDist [pure $ upVol vol, pure $ downVol vol]

data BinomialDist = BinomialDist{ dist :: PDist Double, volatility :: Double}
instance LatticeElement BinomialDist where
    next bd = bd {dist = (binaryStep (volatility bd)) <**> (dist bd)  }

type SimpleModel s = LatticeValueModel s IntDate BinomialDist

makeSimpleModel :: s -> IntDate -> Double -> Double -> SimpleModel s 
makeSimpleModel sym initial_date initial_val volatility = let
    -- pv :: PValue Double
    -- pv = pure initial_val
    pd :: PDist Double
    pd = pure initial_val
    ls :: LatticeSlice IntDate BinomialDist 
    ls = LatticeSlice{date = initial_date, slice = BinomialDist{dist = pd, volatility = volatility} }
    in LatticeValueModel{symbol = sym, model = makeLattice ls }

 