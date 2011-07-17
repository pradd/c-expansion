module Ini where 
import Monad

main = do out <- parseIniFile "galaxy.ini" 
          print out
          
---------------------------------------------------------------------------          
          
parseIniFile fileName =  do contents <- readFile fileName 
                            let out = parseIni contents
                            return out          
          
parseIni :: String -> [(String,String)]
parseIni s = map (split "=") ls where
             ls = removeEmpty $ map trim $ clearComments $ lines s
             
             
clearComments arr = map (firstHalf . split ";") arr

firstHalf :: (String,String) -> String
firstHalf (x, _) = x

split :: String -> String -> (String,String)
split _ []       = ([],[])   
split ch (x:xs)  = if x `elem` ch 
                      then ([],xs)
                      else (x:a,b) where 
                          (a,b) = split ch xs
              
trim :: String -> String              
trim = trimHead . reverse . trimHead . reverse              

trimHead []       = []
trimHead (' ':xs) = trimHead xs
trimHead str      = str

removeEmpty []     = []
removeEmpty (x:xs) = if (length x) > 0
                        then x:(removeEmpty xs)
                        else removeEmpty xs
                        
                        