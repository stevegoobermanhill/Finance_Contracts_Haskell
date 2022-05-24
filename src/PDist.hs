module PDist 
where

import PValue
import Data.List    


--PDist is an array of PValues
--again a monad
--this is really useful
-- applicative allows us to apply a set of functions based on a  probability distribution
-- monad makes that even more useful

data PDist v = PDist{unPDist::[PValue v]}    

instance Functor PDist where
    fmap f ls = PDist $ [fmap f lp | lp <- (unPDist ls)]

instance Applicative PDist where
    pure v = PDist [pure v]
    -- (<*>) :: PDist (a -> b) -> PDist a -> PDist b
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

lift :: (a -> b) -> PDist a -> PDist b
lift = fmap

lift2 :: (a -> b -> c) -> PDist a -> PDist b -> PDist c
lift2 f a b = pure f <*> a <*> b

lift3 :: (a -> b -> c -> d) -> PDist a -> PDist b -> PDist c -> PDist d
lift3 f a b c = pure f <*> a <*> b <*> c


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