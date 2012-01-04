module CExpansion.Turn ( turn ) where

import Config
import CExpansion.Galaxy

turn g = increasePopulation g

increasePopulation = withHumanDetails f
    where   f :: HumanDetails -> HumanDetails
            f hd = hd {population = grow (population hd)}
            grow pop = ceiling ((fromIntegral pop) * 1.02)

