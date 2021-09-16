-- Composable contracts
-- stephen.gooberman-hill@amey.co.uk

-- ValueProcess.hs
-- value process beneath to implement and value contacts 



module ValueProcess (
    PR,
    RV,
    takePr,
    horizonPr,
    andPr
)

where

-- Value processes ---------------
newtype PR a = PR { unPr :: [RV a] } deriving Show
type RV a = [a]

takePr :: Int -> PR a -> PR a
takePr n (PR rvs) = PR $ take n rvs

horizonPr :: PR a -> Int
horizonPr (PR rvs) = length rvs

andPr :: PR Bool -> Bool
andPr (PR rvs) = all and rvs -- and (map and rvs)



