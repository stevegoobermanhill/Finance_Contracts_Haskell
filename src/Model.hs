-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- Model.hs
-- wrapper to build the model

module Model
where

import ValueProcess
import Lattice



-- Model -------------------------
data Model = Model {
                modelStart :: Date,
                disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
                exch       :: Currency -> Currency -> PR Double,
                absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
                rateModel  :: Currency -> PR Double
                }


exampleModel :: CalendarTime -> Model
exampleModel modelDate = Model {
                modelStart = (modelDate,0),
                disc       = disc,
                exch       = exch,
                absorb     = absorb,
                rateModel  = rateModel
                }