module SkyObject ( SkyObject, SkyObjectType, randomSkyObjectsGenerator, placeFactionSkyObject ) where

import Utils ( intToFloat, intToInteger )
import HumanDetails (HumanDetails, placeFactionHumanDetails, defaultHumanDetails)

data SkyObject = SkyObject {humanDetails :: HumanDetails, 
                            objectType :: SkyObjectType
                            -- astroDetails
                            -- geoDetais
                            }
  deriving (Show, Read)

data SkyObjectType = Planet
                   | Belt
  deriving (Show, Read)

randomSkyObjectsGenerator :: [Int] -> [SkyObject]
randomSkyObjectsGenerator (z:rs) = obj : randomSkyObjectsGenerator rs
  where obj = SkyObject {humanDetails = defaultHumanDetails, 
                         objectType = randomSkyObjectType z
                         }

randomSkyObjectType r = if r `mod` 5 == 0 then Belt else Planet

placeFactionSkyObject factionName obj = obj { humanDetails = d }
  where d = placeFactionHumanDetails factionName (humanDetails obj)