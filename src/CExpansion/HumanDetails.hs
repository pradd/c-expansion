module CExpansion.HumanDetails where

data HumanDetails = HumanDetails {faction :: String,
                                  population :: Integer, 
                                  morale :: Float
                                 } deriving (Show, Read)

placeFactionHumanDetails factionName = HumanDetails { faction = factionName, 
                                                      population = 100000000, 
                                                      morale = 0.5 }
