module Init (initGalaxy, initFaction) where

import System.Random ( randoms, newStdGen )
import List ( nub )
import Dao
import SolarSystem --( SolarSystem, coords, skyObjects,   Coords, x, y, z, placeFactionSystem )
import Config
import SkyObject
import HumanDetails
import Utils ( split, split3 )


initGalaxy = do
    seed <- newStdGen
    saveDb (createGalaxy (randoms seed))

createGalaxy :: [Int] -> [SolarSystem]
createGalaxy rs = map (createSystem ns) (createCoordsDistinct ms)
    where (ms, ns) = split rs

-- nub - убрать все повторяющиеся, т.е. превратить list в set
createCoordsDistinct rs = take galaxyInitSystemsNumber (nub (coordsGenerator rs))

coordsGenerator (x:y:z:rs) = c : (coordsGenerator rs)
  where c = Coords { x = (x `mod` galaxySizeX ),   y = (y `mod` galaxySizeY ),   z = (z `mod` galaxySizeZ ) }

createSystem (r:rs) coords = SolarSystem { coords = coords, skyObjects = skyObjects }
  where skyObjects = take (skyObjectsNumber r) (randomSkyObjectsGenerator rs)

-- from 0 to 9 objects in one solar system
skyObjectsNumber r = r `mod` 10

initFaction factionName = do
    galaxy <- loadDb
    saveDb (placeFactionGalaxy factionName galaxy)

placeFactionGalaxy :: String -> [SolarSystem] -> [SolarSystem]
placeFactionGalaxy factionName (sys:galaxy) = (placeFactionSystem factionName sys) : galaxy

