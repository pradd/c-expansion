module CExpansion.SkyObject ( SkyObject, SkyObjectType, randomSkyObjectsGenerator, placeFactionSkyObject, populatedSkyObjects ) where

import Data.Maybe
import CExpansion.Utils ( intToFloat, intToInteger )
import CExpansion.HumanDetails ( HumanDetails, placeFactionHumanDetails )

data SkyObject = SkyObject {humanDetails :: Maybe HumanDetails, 
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
  where obj = SkyObject {humanDetails = Nothing,
                         objectType = randomSkyObjectType z
                         }

randomSkyObjectType r = if r `mod` 5 == 0 then Belt else Planet

placeFactionSkyObject factionName obj = obj { humanDetails = Just d }
  where d = placeFactionHumanDetails factionName

populatedSkyObjects skyObjects = [x | x <- skyObjects, isJust (humanDetails x)]
