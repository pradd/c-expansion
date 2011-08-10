module Turn ( main ) where

import System ( getArgs )
import CExpansion.Dao ( saveDb, loadDb )
import CExpansion.Report ( printFactionInfo )

main = do
    args <- getArgs
    galaxy <- loadDb (head args)
    printFactionInfo (turn galaxy)
    saveDb (args !! 1) (turn galaxy)

turn = id
