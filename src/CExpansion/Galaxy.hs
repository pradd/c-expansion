{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module CExpansion.Galaxy where

import Happstack.State
import Data.Data ( Data, Typeable)
import CExpansion.SolarSystem 
import CExpansion.SkyObject

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
    
    
    
populatedSystems galaxy = [x | x <- galaxy, isSystemPopulated x]



