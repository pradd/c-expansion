module CExpansion.Dao (saveDb, loadDb) where
import CExpansion.SolarSystem

-- TODO: rewrite file on save
saveDb :: String -> [SolarSystem] -> IO()
saveDb dbOut x = writeFile dbOut $ show x

loadDb :: String -> IO [SolarSystem]
loadDb dbIn = do
    file <- readFile dbIn
    return (read file)
    


    
    

