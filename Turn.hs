module Turn ( main ) where

import System ( getArgs )
import List ( length )
import CExpansion.Dao ( saveDb, loadDb )

main = do
    args <- getArgs
    galaxy <- loadDb (head args)
    printFactionInfo (turn galaxy)
    saveDb (args !! 1) (turn galaxy)

turn = id

printFactionInfo galaxy = do writeFile "status.txt" (composeFactionInfo galaxy)

composeFactionInfo galaxy = foldl concat "" list
        where list = map (apply galaxy) [header, body]
              apply gal f = f gal
              concat x y = x ++ "\n" ++ y

header galaxy = factionName ++ (systemsTotal galaxy)

factionName = "Faction name: adfsdfsafsd\n"

systemsTotal galaxy = "Total systems: " ++ show (length galaxy) ++ "\n"
              
body galaxy = "Body=========="
