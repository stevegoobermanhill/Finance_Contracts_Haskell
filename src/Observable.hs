-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

module Observable

where
import ValueProcess
import Date




-- Observable data type -----
newtype Obs a = Obs (Date -> PR a)

instance Show a => Show (Obs a) where
   show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"

-- Primitives over observables ---
konst :: a -> Obs a
konst k = Obs (\t -> bigK k)

lift :: (a -> b) -> Obs a -> Obs b
lift f (Obs o) = Obs (\t -> PR $ map (map f) (unPr $ o t))

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs o1) (Obs o2) = Obs (\t -> PR $ zipWith (zipWith f) (unPr $ o1 t) (unPr $ o2 t))

date :: Obs Date
date = Obs (\t -> PR $ timeSlices [t])

instance Num a => Num (Obs a) where
                    fromInteger i = konst (fromInteger i)
                    (+) = lift2 (+)
                    (-) = lift2 (-)
                    (*) = lift2 (*)
                    abs = lift abs
                    signum = lift signum

instance Eq a => Eq (Obs a) where
                (==) = undefined

(==*) :: Ord a => Obs a -> Obs a -> Obs Bool
(==*) = lift2 (==)

at :: Date -> Obs Bool
at t = date ==* (konst t)

between :: Date -> Date -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= (konst t1)) (date %<= (konst t2))

(%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%<=) = lift2 (<=)
(%=)  = lift2 (==)
(%>=) = lift2 (>=)
(%>)  = lift2 (>)