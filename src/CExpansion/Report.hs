module CExpansion.Report ( printFactionInfo ) where

import CExpansion.Galaxy
import qualified Config ( factionName )

printFactionInfo :: Galaxy -> String -> String
printFactionInfo (Galaxy ss) notifications = 
                         "notifications \n" ++ notifications 
                      ++ "factionName \n" ++ Config.factionName 
                      ++ "systemsTotal \n" ++ (systemsTotal ss)
                      ++ "populatedSystemsTotal \n" ++ (populatedSystemsTotal ss)
                      ++ "populationTotal \n" ++ (populationTotal ss)

systemsTotal galaxy = show (length galaxy)

populatedSystemsTotal galaxy = show (length $ populatedSystemsForGalaxy galaxy)

populationTotal galaxy = show $ sum $ map population $ humanDetailsForGalaxy galaxy
