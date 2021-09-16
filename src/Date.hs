

module Date
where

import Data.Time

type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0