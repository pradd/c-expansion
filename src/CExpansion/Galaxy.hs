{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module CExpansion.Galaxy where

import Happstack.State
import Data.Data ( Data, Typeable)
import CExpansion.Utils

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
    

populatedSkyObjects skyObjects = [x | x <- skyObjects, isJust (humanDetails x)]
  where isJust Nothing = False
        isJust _ = True

isSystemPopulated sys = length (populatedSkyObjects (skyObjects sys)) > 0

populatedSystems galaxy = [x | x <- galaxy, isSystemPopulated x]



