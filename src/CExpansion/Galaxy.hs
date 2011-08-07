module CExpansion.Galaxy where

import CExpansion.SolarSystem 
import CExpansion.SkyObject

type Galaxy = [SolarSystem]

populatedSystems galaxy = [x | x <- galaxy, isSystemPopulated x]



