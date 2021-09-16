import System.IO

import Environment
import Contract
import DerivedContract
import Model
import Valuation

main :: IO ExitCode
main = do
    xm :: Model
    xm = exampleModel ()

    evalX :: Contract -> PR Double
    evalX = evalC xm USD

    -- Examples -----------------------------------


    zcb :: Date -> Double -> Currency -> Contract
    zcb t x k = cWhen (at t) (scale (konst x) (one k))

    c1 :: Contract
    c1 = zcb t1 10 USD

    t1 :: Date
    t1 = mkDate t1Horizon

    t1Horizon = 3 :: TimeStep

    c11 :: Contract
    c11 = european (mkDate 2)
                (zcb (mkDate 20) 0.4 USD `cAnd`
                zcb (mkDate 30) 9.3 USD `cAnd`
                zcb (mkDate 40) 109.3 USD `cAnd`
                give (zcb (mkDate 12) 100 USD))


    pr1 :: PR Double
    pr1 = evalX c1

    tr1 = unPr pr1

    future:: Date -> Equity -> Double -> Double -> Currency -> Contract
    future t equity quantity price currency = cWhen (at t) (scale (konst quantity) (OneE equity)) `cAnd` Give ((scale (konst price) (One currency)))

    f1= future (mkDate 3 0) IBM 100 2000.0 USD

    putStrLn show tr1