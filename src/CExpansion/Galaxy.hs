{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module CExpansion.Galaxy where

import Happstack.State
import Data.Data ( Data, Typeable)
import Data.Maybe ( isJust, mapMaybe )
import Control.Monad ( liftM )

data HumanDetails = HumanDetails {faction :: String,
                                  population :: Integer, 
                                  morale :: Float
                                 } deriving (Eq, Read, Show, Data, Typeable) 

instance Version HumanDetails
$(deriveSerialize ''HumanDetails)

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

data Coords = Coords {x :: Int, y :: Int, z :: Int}
  deriving (Eq, Read, Show, Data, Typeable) 

instance Version Coords
$(deriveSerialize ''Coords)

data SolarSystem = SolarSystem { coords :: Coords , skyObjects :: [SkyObject] }
  deriving (Eq, Read, Show, Data, Typeable) 
  
instance Version SolarSystem
$(deriveSerialize ''SolarSystem)

newtype Galaxy = Galaxy [SolarSystem]
    deriving (Eq, Read, Show, Data, Typeable)

instance Version Galaxy
$(deriveSerialize ''Galaxy)

data AppState = AppState {
      galaxy :: Galaxy
    } deriving (Eq, Read, Show, Data, Typeable)
    
instance Version AppState
$(deriveSerialize ''AppState)
    
instance Component AppState where
    type Dependencies AppState = End
    initialValue = AppState { galaxy = Galaxy [] }


populatedSystemsForGalaxy :: [SolarSystem] -> [SolarSystem]
populatedSystemsForGalaxy = filter isSystemPopulated
    where isSystemPopulated sys = hasPopulatedSkyObjects (skyObjects sys)
          hasPopulatedSkyObjects = any (isJust . humanDetails)

populatedSkyObjectsForGalaxy :: [SolarSystem] -> [SkyObject]
populatedSkyObjectsForGalaxy ss = concatMap skyObjects (populatedSystemsForGalaxy ss)

humanDetailsForSkyObjects :: [SkyObject] -> [HumanDetails]
humanDetailsForSkyObjects = mapMaybe humanDetails

humanDetailsForGalaxy :: [SolarSystem] -> [HumanDetails]
humanDetailsForGalaxy = humanDetailsForSkyObjects . populatedSkyObjectsForGalaxy

withSolarSystems :: (SolarSystem -> SolarSystem) -> Galaxy -> Galaxy
withSolarSystems f (Galaxy ss) = Galaxy (map f ss)

withSkyObjects :: (SkyObject -> SkyObject) -> Galaxy -> Galaxy
withSkyObjects f = withSolarSystems f' 
        where f' :: SolarSystem -> SolarSystem
              f' ss@(SolarSystem {skyObjects = so}) = ss { skyObjects = map f so }

withHumanDetails :: (HumanDetails -> HumanDetails) -> Galaxy -> Galaxy
withHumanDetails f = withSkyObjects f' 
        where f' :: SkyObject -> SkyObject
              f' so@(SkyObject {humanDetails = hd}) = so { humanDetails = liftM f hd }
