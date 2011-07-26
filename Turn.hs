module Turn ( main ) where

import System ( getArgs )
import Data.List ( length )
import CExpansion.Dao ( saveDb, loadDb )
import CExpansion.Galaxy
import qualified Config ( factionName )

main = do
    args <- getArgs
    galaxy <- loadDb (head args)
    printFactionInfo (turn galaxy)
    saveDb (args !! 1) (turn galaxy)

turn = id

printFactionInfo galaxy = writeFile "report.txt" (composeFactionInfo galaxy)

composeFactionInfo galaxy = foldl concat "" parts
        where parts = map ($ galaxy) reportStructure
              concat x y = x ++ "\n" ++ y

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
