-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

module Contract

where

import Observable
import Environment


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



