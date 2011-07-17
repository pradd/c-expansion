module Dao (saveDb, loadDb) where
import SolarSystem

dbName = "galaxy.db"

-- TODO: rewrite file on save
saveDb :: [SolarSystem] -> IO()
saveDb x = writeFile dbName $ show x

loadDb :: IO([SolarSystem])
loadDb = do
    file <- readFile dbName
    return (read file)
    


    
    

