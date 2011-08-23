{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module CExpansion.HumanDetails where

import Data.Data ( Data, Typeable)
import Happstack.State

data HumanDetails = HumanDetails {faction :: String,
                                  population :: Integer, 
                                  morale :: Float
                                 } deriving (Eq, Read, Show, Data, Typeable) 

instance Version HumanDetails
$(deriveSerialize ''HumanDetails)

placeFactionHumanDetails factionName = HumanDetails { faction = factionName, 
                                                      population = 100000000, 
                                                      morale = 0.5 }
