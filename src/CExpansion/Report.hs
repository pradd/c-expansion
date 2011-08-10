module CExpansion.Report ( printFactionInfo ) where

import Data.List ( length, intercalate )
import CExpansion.Galaxy
import qualified Config ( factionName )

printFactionInfo galaxy = writeFile "report.txt" (composeFactionInfo galaxy)

composeFactionInfo galaxy = intercalate "\n" parts
        where parts = map ($ galaxy) reportStructure

reportStructure = [
                    -- header    
                    factionName
                  , systemsTotal
                  , populatedSystemsTotal
                  , reportPartsBreak
                    -- body
                  , body
                  ]

reportPartsBreak _ = "=================="

factionName _ = "Faction name: " ++ Config.factionName ++ "\n"

systemsTotal galaxy = "Total systems: " ++ show (length galaxy) ++ "\n"

populatedSystemsTotal galaxy = "Populated systems: " ++ show (length $ populatedSystems galaxy) ++ "\n"
              
body galaxy = "Body=========="
