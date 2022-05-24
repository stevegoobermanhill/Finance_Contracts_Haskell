{-# LANGUAGE InstanceSigs #-}

module PValue 
where

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

lift :: (a -> b) -> PValue a -> PValue b
lift = fmap

lift2 :: (a -> b -> c) -> PValue a -> PValue b -> PValue c
lift2 f a b = pure f <*> a <*> b

lift3 :: (a -> b -> c -> d) -> PValue a -> PValue b -> PValue c -> PValue d
lift3 f a b c = pure f <*> a <*> b <*> c