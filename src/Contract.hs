-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

module Contract

where

import Data.Time

import ValueProcess

type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0


-- Observable data type -----
newtype Obs a = Obs (Date -> PR a)

instance Show a => Show (Obs a) where
   show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"



-- Contract primitives ------
data Contract = Zero
                | One  Currency
		          | OneE  Equity
                | Give Contract
                | And  Contract Contract
                | Or   Contract Contract
                | Cond    (Obs Bool)   Contract Contract
                | Scale   (Obs Double) Contract
                | When    (Obs Bool)   Contract
                | Anytime (Obs Bool)   Contract
                | Until   (Obs Bool)   Contract
                deriving Show


zero :: Contract
zero = Zero

one :: Currency -> Contract
one = One

give :: Contract -> Contract
give = Give

cAnd :: Contract -> Contract -> Contract
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

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

