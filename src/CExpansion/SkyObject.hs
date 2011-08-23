{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module CExpansion.SkyObject ( SkyObject, SkyObjectType, randomSkyObjectsGenerator, placeFactionSkyObject, populatedSkyObjects ) where

import Data.Maybe
import Data.Data ( Data, Typeable)
import Happstack.State
import CExpansion.Utils ( intToFloat, intToInteger )
import CExpansion.HumanDetails ( HumanDetails, placeFactionHumanDetails )

data SkyObjectType = Planet
                   | Belt
  deriving (Eq, Read, Show, Data, Typeable) 

instance Version SkyObjectType
$(deriveSerialize ''SkyObjectType)

data SkyObject = SkyObject {humanDetails :: Maybe HumanDetails, 
                            skyObjectType :: SkyObjectType
                            -- astroDetails
                            -- geoDetais
                            }
  deriving (Eq, Read, Show, Data, Typeable) 

instance Version SkyObject
$(deriveSerialize ''SkyObject)

randomSkyObjectsGenerator :: [Int] -> [SkyObject]
randomSkyObjectsGenerator (z:rs) = obj : randomSkyObjectsGenerator rs
  where obj = SkyObject {humanDetails = Nothing,
                         skyObjectType = randomSkyObjectType z
                         }

randomSkyObjectType r = if r `mod` 5 == 0 then Belt else Planet

placeFactionSkyObject factionName obj = obj { humanDetails = Just d }
  where d = placeFactionHumanDetails factionName

populatedSkyObjects skyObjects = [x | x <- skyObjects, isJust (humanDetails x)]
