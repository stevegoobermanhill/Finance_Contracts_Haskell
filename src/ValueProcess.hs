-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- ValueProcess.hs
-- value process beneath to implement and value contacts 



module ValueProcess
where
    
import Date
-- Value processes ---------------
newtype PR a = PR { unPr :: [RV a] } deriving Show
type RV a = [a]

takePr :: Int -> PR a -> PR a
takePr n (PR rvs) = PR $ take n rvs

horizonPr :: PR a -> Int
horizonPr (PR rvs) = length rvs

andPr :: PR Bool -> Bool
andPr (PR rvs) = all and rvs -- and (map and rvs)

-- Process primitives    
bigK :: a -> PR a
bigK x = PR (konstSlices x)

konstSlices :: a -> [[a]]
konstSlices x = nextSlice [x]
   where nextSlice sl = sl : (nextSlice (x:sl))

datePr :: PR Date
datePr = PR $ timeSlices [time0]

timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

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

