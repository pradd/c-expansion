module CExpansion.Turn ( turn ) where

import CExpansion.Galaxy

turn :: Galaxy -> Galaxy
turn = increasePopulation

increasePopulation :: Galaxy -> Galaxy
increasePopulation = withHumanDetails f
    where   f :: HumanDetails -> HumanDetails
            f hd@(HumanDetails {population = p}) = hd {population = grow p}
            grow pop = ceiling (fromIntegral pop * 1.02 :: Double)

