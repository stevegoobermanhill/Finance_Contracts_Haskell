-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- DerivedContract.hs
-- additional (non atomic) contract definitions

module DerivedContract
where

import Contract

-- Derived primitive -------------
andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d


-- Option contracts --------------
european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) u = anytime (between t1 t2) u