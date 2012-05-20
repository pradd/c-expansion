module CExpansion.Init ( initGalaxy ) where

import Data.List ( nub )
import Config
import CExpansion.Galaxy
import CExpansion.Utils ( split )

initGalaxy randoms = Galaxy $ placeFactionInGalaxy factionName $ createGalaxy randoms

createGalaxy :: [Int] -> [SolarSystem]
createGalaxy rs = map (createSystem ns) (createCoordsDistinct ms)
    where (ms, ns) = split rs

-- nub - убрать все повторяющиеся, т.е. превратить list в set
createCoordsDistinct rs = take galaxyInitSystemsNumber (nub (coordsGenerator rs))

coordsGenerator (x:y:z:rs) = c : coordsGenerator rs
  where c = Coords { x = x `mod` galaxySizeX , y = y `mod` galaxySizeY , z = z `mod` galaxySizeZ }

createSystem (r:rs) coords = SolarSystem { coords = coords, skyObjects = skyObjects }
  where skyObjects = take num (randomSkyObjectsGenerator rs)
        -- from 1 to 10 objects in one solar system
        num = r `mod` 10 + 1

placeFactionInGalaxy :: String -> [SolarSystem] -> [SolarSystem]
placeFactionInGalaxy factionName (sys:galaxy) = placeFactionInSystem factionName sys : galaxy

randomSkyObjectsGenerator :: [Int] -> [SkyObject]
randomSkyObjectsGenerator (z:rs) = obj : randomSkyObjectsGenerator rs
  where obj = SkyObject {humanDetails = Nothing,
                         skyObjectType = randomSkyObjectType z
                         }

randomSkyObjectType r = if r `mod` 5 == 0 then Belt else Planet

placeFactionHumanDetails factionName = HumanDetails { faction = factionName, 
                                                      population = 100000000, 
                                                      morale = 0.5 }

placeFactionSkyObject factionName obj = obj { humanDetails = Just d }
  where d = placeFactionHumanDetails factionName

placeFactionInSystem factionName system = system { skyObjects = s }
  where s = placeFactionSkyObject factionName (head (skyObjects system)) : tail (skyObjects system)
