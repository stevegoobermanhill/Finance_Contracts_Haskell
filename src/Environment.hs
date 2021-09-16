-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- Environment.hs
-- environmental values

module Environment (
    CValue,
    Currency(..),
    Equity(..),
    rateModel,
    equityModel
)
where

import ValueProcess

data Currency = USD | GBP | EUR | ZAR | KYD | CHF  
                deriving (Eq, Show)

data Equity = IBM | DEL 
                deriving (Eq, Show)

-- currency model
type CValue = Double

data ValueModel a = ValueModel {unVM :: (a, (CValue, CValue))} deriving Show

values :: ValueModel a -> (a, PR CValue)
values vm = (v, PR $ makeRateSlices rateNow 1)
    where
        (v, (rateNow, delta)) = unVM vm
        makeRateSlices rateNow n = (rateSlice rateNow n) : (makeRateSlices (rateNow-delta) (n+1))
        rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]


currencyModels = [ ValueModel (CHF, (7,   0.8))
                , ValueModel (EUR, (6.5, 0.25))
                , ValueModel (GBP, (8,   0.5))
                , ValueModel (KYD, (11,  1.2))
                , ValueModel (USD, (5,  1))
                , ValueModel (ZAR, (15,  1.5)) ]

-- rates = values Currency

rateModels = map values currencyModels


rateModel :: Currency -> PR CValue
rateModel k = case lookup k rateModels of
                Just x -> x
                Nothing -> error $ "rateModel: currency not found " ++ (show k)


equityModels = [ ValueModel (IBM, (100.0, 1.0)),
                 ValueModel (DEL, (50.0, 1.5))]

equityModels' = map values equityModels

equityModel :: Equity -> PR CValue
equityModel k = case lookup k equityModels' of
                Just x -> x
                Nothing -> error $ "equityModel: equity not found " ++ (show k)