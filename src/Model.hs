-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- Model.hs
-- wrapper to build the model

module Model
where

import Date
import ValueProcess
import Lattice as L
import Observable
import Environment as E



-- Model -------------------------
data Model = Model {
                modelStart :: Date,
                disc       :: Currency -> (PR Bool, PR CValue) -> PR CValue,
                exch       :: Currency -> Currency -> PR CValue,
                absorb     :: Currency -> (PR Bool, PR CValue) -> PR CValue,
                rateModel  :: Currency -> PR CValue
                }


exampleModel :: CalendarTime -> Model
exampleModel modelDate = Model {
                modelStart = (modelDate,0),
                Model.disc       = L.disc,
                Model.exch       = L.exch,
                Model.absorb     = L.absorb,
                Model.rateModel  = E.rateModel
                }