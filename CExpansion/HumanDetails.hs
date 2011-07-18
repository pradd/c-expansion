module CExpansion.HumanDetails where

data HumanDetails = HumanDetails {faction :: String,
                                  population :: Integer, 
                                  morale :: Float
                                 } deriving (Show, Read)

defaultHumanDetails = HumanDetails { faction = "", population = 0, morale = 0.0 }

placeFactionHumanDetails factionName humanDetails = HumanDetails { faction = factionName, 
                                                                   population = 100000000, 
                                                                   morale = 0.5 }
