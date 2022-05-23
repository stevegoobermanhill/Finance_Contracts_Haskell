{-# LANGUAGE InstanceSigs #-}

module PDist 
where

import Data.List    
-- can we make the lattice a monad ?


--PValue is a pair of (prob, value)
--we can make it a monad :-)
data PValue v = PValue{prob::Double, value::v}

instance Functor PValue where
    fmap :: (a -> b) -> PValue a -> PValue b 
    fmap f lp = PValue {prob = prob lp, value = f $ value lp}

instance Applicative PValue where
    pure v = PValue{prob = 1.0, value = v}
    lpf <*> lpv = PValue{prob = (prob lpf) * (prob lpv), value = (value lpf) $ (value lpv)}

instance Monad PValue where
    return = pure
    -- PValue v >>= (v -> PValue w) -> PValue w
    lp >>= f = PValue{prob = p, value = v} where
        l = f $ value lp
        p = prob lp * prob l
        v = value l 

-- if v is an instance of Eq then PValue is an instance of Eq
instance (Eq v) => Eq(PValue v) where
    (==) x y = value x == value y


--PDist is an array of PValues
--again a monad
--this is really useful
-- applicative allows us to apply a set of functions based ona  probability distribution
-- monad makes that even more useful

data PDist v = PDist{unPDist::[PValue v]}    

instance Functor PDist where
    fmap f ls = PDist $ [fmap f lp | lp <- (unPDist ls)]

instance Applicative PDist where
    pure v = PDist [pure v]
    (<*>) :: PDist (a -> b) -> PDist a -> PDist b
    lsf <*> lsv = PDist lps where
        lps = [ lpf <*> lpv | lpf <- unPDist lsf, lpv <- unPDist lsv]
    
instance Monad PDist where
    return = pure
    -- PDist v >>= (v -> PDist w) -> PDist w
    ls >>= f = PDist lps where


        lss =  [map (\lpw -> PValue{prob = prob lpv * prob lpw, value = value lpw})
                    (unPDist $ (f $ value lpv))
                    | lpv <-  unPDist ls]
        lps = concat lss


-- The functor / applicative / monad functionality on PDist doesn't enable us to group
-- together elements with identical values
-- so compact will enable that
compact :: (Eq v) => PDist v -> PDist v
compact lsv = PDist ns where
    groups = groupBy (\x y -> value x == value y) (unPDist lsv)
    ns = map (\g -> PValue{prob = sum $ map (\lp -> prob lp) g, value = value $ head g} ) groups

-- normalize ensures that the total prob across a PDist adds to 1.0
normalize :: PDist v -> PDist v
normalize ls = PDist [lp{prob = prob lp / total} | lp <- unPDist ls] where
    total = sum$ map (\lp -> prob lp) (unPDist ls)

-- compNorm is simply shorthand :-)
compNorm :: (Eq v) => PDist v -> PDist v
compNorm = normalize . compact 

-- <**> is an extension of the applicative <*> to compactable PDists
-- can't be in Applicative as it would break the Applicative laws
(<**>) :: (Eq b) => PDist (a -> b) -> PDist a -> PDist b
lsf <**> lsv = compNorm $ lsf <*> lsv


-- reachable
-- given an applicative style mapping of a
-- PDist a->b, a PDist a and a PDist b
-- will return the subset of PDist a is the pre-image of PDist b
-- under PDist a->b
reachable ::(Eq b) => PDist (a -> b) -> PDist a -> PDist b ->  PDist a
reachable f xs yy = let
    yVals = map value $ unPDist yy
    xVals = [ (x, map value $ unPDist (f <*> PDist [x]) )| x <- unPDist xs]
    in
    PDist [ x | (x, vals) <- xVals, not . null $ intersect vals yVals ]    