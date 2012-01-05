module CExpansion.Turn ( turn ) where

import Config
import CExpansion.Galaxy

turn g = increasePopulation g

increasePopulation = withHumanDetails f
    where   f :: HumanDetails -> HumanDetails
            f hd@(HumanDetails {population = p}) = hd {population = grow p}
            grow pop = ceiling ((fromIntegral pop) * 1.02)

