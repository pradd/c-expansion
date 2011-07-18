module InitGalaxy where

import System.Random ( randoms, newStdGen )
import Data.List ( nub )
import CExpansion.Dao ( saveDb )
import CExpansion.SolarSystem
import Config
import CExpansion.SkyObject
import CExpansion.HumanDetails
import CExpansion.Utils ( split )

main = initGalaxy

initGalaxy = do
    seed <- newStdGen
    saveDb (placeFactionInGalaxy factionName $ createGalaxy (randoms seed))

createGalaxy :: [Int] -> [SolarSystem]
createGalaxy rs = map (createSystem ns) (createCoordsDistinct ms)
    where (ms, ns) = split rs

-- nub - убрать все повторяющиеся, т.е. превратить list в set
createCoordsDistinct rs = take galaxyInitSystemsNumber (nub (coordsGenerator rs))

coordsGenerator (x:y:z:rs) = c : coordsGenerator rs
  where c = Coords { x = x `mod` galaxySizeX , y = y `mod` galaxySizeY , z = z `mod` galaxySizeZ }

createSystem (r:rs) coords = SolarSystem { coords = coords, skyObjects = skyObjects }
  where skyObjects = take (skyObjectsNumber r) (randomSkyObjectsGenerator rs)

-- from 0 to 9 objects in one solar system
skyObjectsNumber r = r `mod` 10

placeFactionInGalaxy :: String -> [SolarSystem] -> [SolarSystem]
placeFactionInGalaxy factionName (sys:galaxy) = placeFactionInSystem factionName sys : galaxy

