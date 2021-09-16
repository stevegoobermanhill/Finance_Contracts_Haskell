-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- DerivedContract.hs
-- additional (non atomic) contract definitions

module DerivedContract
where

import Environment
import Contract
import Observable
import Date

-- Derived primitive -------------
andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d


-- Option contracts --------------
european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) u = anytime (between t1 t2) u

-- zero coupon bond
zcb :: Date -> CValue -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))

--futures contract
future:: Date -> Equity -> Double -> CValue -> Currency -> Contract
future t equity quantity price currency = cWhen (at t) ((scale (konst quantity) (OneE equity)) `cAnd` Give ((scale (konst price) (One currency))))
