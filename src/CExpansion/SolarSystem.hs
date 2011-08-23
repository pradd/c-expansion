{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module CExpansion.SolarSystem where

import CExpansion.SkyObject
import Data.Data ( Data, Typeable)
import Happstack.State

data Coords = Coords {x :: Int, y :: Int, z :: Int}
  deriving (Eq, Read, Show, Data, Typeable) 

instance Version Coords
$(deriveSerialize ''Coords)

data SolarSystem = SolarSystem { coords :: Coords , skyObjects :: [SkyObject] }
  deriving (Eq, Read, Show, Data, Typeable) 
  
instance Version SolarSystem
$(deriveSerialize ''SolarSystem)

placeFactionInSystem factionName system = system { skyObjects = s }
  where s = placeFactionSkyObject factionName (head (skyObjects system)) : tail (skyObjects system)

isSystemPopulated sys = length (populatedSkyObjects (skyObjects sys)) > 0

