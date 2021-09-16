import System.IO

import Date
import Environment
import Contract
import DerivedContract
import Model
import Valuation

main :: IO ()
main = do
    -- xm :: Model
    let xm = exampleModel ()

    -- evalX :: Contract -> PR CValue
    let evalX = evalC xm USD

    -- Examples -----------------------------------
   -- let t1Horizon = 3 :: TimeStep
    let t1 = mkDate 3
    let c1 = zcb t1 10 USD

    let c11 = european (mkDate 2)
                (zcb (mkDate 20) 0.4 USD `cAnd`
                zcb (mkDate 30) 9.3 USD `cAnd`
                zcb (mkDate 40) 109.3 USD `cAnd`
                give (zcb (mkDate 12) 100 USD))


    let f1= future (mkDate 3) IBM 100.0 2000.0 USD

    let pr1 = evalX f1
    --let tr1 = unPr pr1

    

 
    putStrLn $ show pr1