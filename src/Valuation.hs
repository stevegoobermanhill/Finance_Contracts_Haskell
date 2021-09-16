-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- Valuation.hs
-- Valuation of a contract 

module Valuation (
    evalC
)
where

import ValueProcess
import Model
import Environment
import Contract


-- Compositional valuation semantics for contracts    
evalC :: Model -> Currency -> Contract -> PR Double
evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
    where   eval Zero           = bigK 0
            eval (One k2)       = exch k k2
            eval (Give c)       = -(eval c)
            eval (o `Scale` c)  = (evalO o) * (eval c)
            eval (c1 `And` c2)  = (eval c1) + (eval c2)
            eval (c1 `Or` c2)   = max (eval c1) (eval c2)
            eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
            eval (When o c)     = disc   k (evalO o, eval c)
    --      eval (Anytime o c)  = snell  k (evalO o, eval c)
            eval (Until o c)    = absorb k (evalO o, eval c)


-- Valuation semantics for observables    
evalO :: Obs a -> PR a
evalO (Obs o) = o time0


-- Process primitives    
bigK :: a -> PR a
bigK x = PR (konstSlices x)

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
