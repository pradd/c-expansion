module CExpansion.SolarSystem where

import CExpansion.SkyObject ( SkyObject, placeFactionSkyObject, populatedSkyObjects )

data SolarSystem = SolarSystem { coords :: Coords , skyObjects :: [SkyObject] }
  deriving (Show, Read)
  
data Coords = Coords {x :: Int, y :: Int, z :: Int}
  deriving (Show, Read, Eq) 

placeFactionInSystem factionName system = system { skyObjects = s }
  where s = placeFactionSkyObject factionName (head (skyObjects system)) : tail (skyObjects system)

isSystemPopulated sys = length (populatedSkyObjects (skyObjects sys)) > 0

