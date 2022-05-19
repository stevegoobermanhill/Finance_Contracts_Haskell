{-# LANGUAGE InstanceSigs #-}

module LatticeSlice 
where

import Data.List    
-- can we make the lattice a monad ?


--LatticePoint is a pair of (prob, value)
--we can make it a monad :-)
data LatticePoint v = LatticePoint{prob::Double, value::v}

instance Functor LatticePoint where
    fmap :: (a -> b) -> LatticePoint a -> LatticePoint b 
    fmap f lp = LatticePoint {prob = prob lp, value = f $ value lp}

instance Applicative LatticePoint where
    pure v = LatticePoint{prob = 1.0, value = v}
    lpf <*> lpv = LatticePoint{prob = (prob lpf) * (prob lpv), value = (value lpf) $ (value lpv)}

instance Monad LatticePoint where
    return = pure
    -- LatticePoint v >>= (v -> LatticePoint w) -> LatticePoint w
    lp >>= f = LatticePoint{prob = p, value = v} where
        l = f $ value lp
        p = prob lp * prob l
        v = value l 

--LatticeSlice is an array of LatticePoints
--again a monad
--this is really useful
-- applicative allows us to apply a set of functions based ona  probability distribution
-- monad makes that even more useful

data LatticeSlice v = LatticeSlice{unslice::[LatticePoint v]}    

instance Functor LatticeSlice where
    fmap f ls = LatticeSlice $ [fmap f lp | lp <- (unslice ls)]

instance Applicative LatticeSlice where
    pure v = LatticeSlice [pure v]
    (<*>) :: LatticeSlice (a -> b) -> LatticeSlice a -> LatticeSlice b
    lsf <*> lsv = LatticeSlice lps where
        lps = [ lpf <*> lpv | lpf <- unslice lsf, lpv <- unslice lsv]
    
instance Monad LatticeSlice where
    return = pure
    -- LatticeSlice v >>= (v -> LatticeSlice w) -> LatticeSlice w
    ls >>= f = LatticeSlice lps where


        lss =  [map (\lpw -> LatticePoint{prob = prob lpv * prob lpw, value = value lpw})
                    (unslice $ (f $ value lpv))
                    | lpv <-  unslice ls]
        lps = concat lss

-- The functor / applicative / monad functionality on LatticeSlice doesn't enable us to group
-- together elements with identical values
-- so compact will enable that
compact :: (Eq v) => LatticeSlice v -> LatticeSlice v
compact lsv = LatticeSlice ns where
    groups = groupBy (\x y -> value x == value y) (unslice lsv)
    ns = map (\g -> LatticePoint{prob = sum $ map (\lp -> prob lp) g, value = value $ head g} ) groups

-- normalize ensures that the total prob across a LatticeSlice adds to 1.0
normalize :: LatticeSlice v -> LatticeSlice v
normalize ls = LatticeSlice [lp{prob = prob lp / total} | lp <- unslice ls] where
    total = sum$ map (\lp -> prob lp) (unslice ls)

-- compNorm is simply shorthand :-)
compNorm :: (Eq v) => LatticeSlice v -> LatticeSlice v
compNorm = normalize . compact 