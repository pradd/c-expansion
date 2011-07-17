module SolarSystem where

import SkyObject (SkyObject, placeFactionSkyObject)

data SolarSystem = SolarSystem { coords :: Coords , skyObjects :: [SkyObject] }
  deriving (Show, Read)
  
data Coords = Coords {x :: Int, y :: Int, z :: Int}
  deriving (Show, Read, Eq) 

placeFactionSystem factionName system = system { skyObjects = s }
  where s = (placeFactionSkyObject factionName (head (skyObjects system))) : (tail (skyObjects system))