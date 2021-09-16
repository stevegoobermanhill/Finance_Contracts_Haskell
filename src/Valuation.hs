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
import Observable
import Date


-- Compositional valuation semantics for contracts    
evalC :: Model -> Currency -> Contract -> PR CValue
evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
    where   eval Zero           = bigK 0
            eval (One k2)       = exch k k2
            eval (OneE e)       = equityModel e
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



