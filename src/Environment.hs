-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- Environment.hs
-- environmental values

module Environment (
    CValue,
    Currency(..),
    Equity(..),
    rateModel
)
where

import ValueProcess

data Currency = USD | GBP | EUR | ZAR | KYD | CHF  
                deriving (Eq, Show)

data Equity = IBM | DEL 
                deriving (Eq, Show)

-- currency model
type CValue = Double

newtype CurrencyModel = CurrencyModel {unCM :: (Currency, (CValue, CValue))} -- Currency, (Initial Value, Delta)
    deriving Show


currencyModels = [ CurrencyModel (CHF, (7,   0.8))
                , CurrencyModel (EUR, (6.5, 0.25))
                , CurrencyModel (GBP, (8,   0.5))
                , CurrencyModel (KYD, (11,  1.2))
                , CurrencyModel (USD, (5,  1))
                , CurrencyModel (ZAR, (15,  1.5)) ]

rateModels = map rates currencyModels


rates :: CurrencyModel -> (Currency, PR CValue)
rates cm = (c, PR $ makeRateSlices rateNow 1)
    where
        (c, (rateNow, delta)) = unCM cm
        makeRateSlices rateNow n = (rateSlice rateNow n) : (makeRateSlices (rateNow-delta) (n+1))
        rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]

rateModel :: Currency -> PR CValue
rateModel k = case lookup k rateModels of
                Just x -> x
                Nothing -> error $ "rateModel: currency not found " ++ (show k)

