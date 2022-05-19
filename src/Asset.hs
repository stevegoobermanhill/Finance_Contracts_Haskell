-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- Asset management processes

module Asset

where

import ValueProcess
import Date
import Observable

data Component = Chassis | Transformer | Controller | Motor deriving Show

-- condition is an integer, 0 is minimum condition (asset failed)
type ConditionValue = Int
type CProb = Double 
maxCondition = 9
repairedCondition = 6

condition::[ConditionValue]
condition = [0..maxCondition]

actualCondition:: PR CProb
actualCondition = PR $ degradation []

-- asset installation
installation :: RV CProb
installation = 1.0 : replicate maxCondition 0.0 

-- simple uniform degradation model : rate = 1-step degradation prob 
degradation :: RV CProb -> CProb -> RV CProb
degradation [] = installation 
degradation x = zipWith (\a b -> a+b) (map (\x -> x * (1.0-rate)) x) init (0.0:(map (\x -> x * rate) x))

type ConditionObs = Obs CProb
data Condition = {actual:: ConditionObs, known::Obs CProb}
type Asset = (Component, Condition)


-- operations on the Actual asset state

-- fail returns the failure probability, which is simply the probability that Condition == 0
fail :: Asset -> Obs CProb
fail (_, c) = Obs $ (\t -> failed $ actual c ) where
    failed p = map (\x -> replicate maxCondition 0.0 : tail x) (unPr p) 

-- replace installs a new asset, so changes the 
replace :: RV CProb -> RV CProb
replace _ = installation

repair :: RV CProb -> RV CProb
repair actual 

-- intervention applies a function (action) on an RV to a PR where the schedule Observable is True
intervention :: Asset -> Obs Bool -> (RV CProb -> RV CProb) -> Asset
intervention asset@(name, c) schedule action = (name, newCondition) where
    newCondition = (Obs $ \t ->newActual, known)
    interveneProb = sum $ zipWith (\b x -> if b then x else 0.0) schedule known
    -- newActual =  (condPr schedule (map action (actual c)) (actual c)) -- apply the action (which will alter probabilities) if schedule says so
    newActual = zipWith (\a b -> a + b) (map (* interveneProb) $ map action (actual c)) (map (* (1.0-interveneProb)) (actual c))
-- operations on the Known Asset State

-- Inspection replaces the Known state with the Actual state if the schedule Observable is True
-- weighted by the probabilities of the schedule being true / false 
inspection :: Asset -> Obs Bool -> Asset
inspection asset@(name, c) schedule = (name, newCondition) where
    newCondition = (actual, Obs $ (\t -> newKnown)
    -- prob of updating the condition is the sum of the known probs where the schedule is True 
    inspectProb = sum $ zipWith (\b x -> if b then x else 0.0) schedule known
    newKnown = zipWith (\a b -> a + b) (map (* inspectProb) actual) (map (* (1.0-inspectProb)) known c)
        -- sum over the 
        


-- a planned inspection schedule always takes place on the same date
plannedInspectionSchedule :: TimeStep -> PR Bool
plannedInspectionSchedule t = [repeat maxCondition ts%t == 0 | ts <- [time0 ..]]






