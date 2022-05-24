-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- ValueProcess.hs
-- value process beneath to implement and value contacts 



module ValueProcess2
where
    
import Date
import Lattice
-- Value processes ---------------
-- newtype PR a = PR { unPr :: [RV a] } deriving Show
-- type RV a = [a]
-- type PR = Lattice d v
-- type RV = PDist Double

takePr :: Int -> Lattice d v -> Lattice d v
takePr n pdists = take n pdists

horizonPr :: Lattice d v -> Int
horizonPr pdists = length pdists

andPr :: Lattice d Bool -> Bool
-- andPr (PR rvs) = all and rvs -- and (map and rvs)
andPr pdists = all lift and pdists 

-- Process primitives    
bigK ::  d-> a -> Lattice d a 
bigK start x = let
    ls :: LatticeSlice d a 
    ls = LatticeSlice{date = start, slice = pure x}
    in makeLatticeFn id ls

datePr :: d -> Lattice d d
datePr start = let
    ls = LatticeSlice{date = start, slice = pure date}
    in makeLatticeFn id ls

condPr :: Lattice d Bool -> Lattice d a -> Lattice d a -> Lattice d a
condPR b t f = let
    b = slice b
    t = slice t
    f = slice f

condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = lift3Pr (\b tru fal -> if b then tru else fal)

liftPr :: (a -> b) -> PR a -> PR b
liftPr f (PR a) = PR $ map (map f) a

lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
zipWithAll f as@(_:_) []       = as
zipWithAll f []       bs@(_:_) = bs
zipWithAll _ _        _        = []


instance Num a => Num (PR a) where
    fromInteger i = bigK (fromInteger i)
    (+) = lift2PrAll (+)
    (-) = lift2PrAll (-)
    (*) = lift2PrAll (*)
    abs = liftPr  abs
    signum = liftPr signum

instance Ord a => Ord (PR a) where
    max = lift2Pr max

instance Eq a => Eq (PR a) where
    (PR a) == (PR b) = a == b

