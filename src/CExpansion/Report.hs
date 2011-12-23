module CExpansion.Report ( printFactionInfo ) where

import Text.StringTemplate
import Data.List ( length, intercalate )
import CExpansion.Galaxy
import qualified Config ( factionName )

printFactionInfo templateText g = let template = newSTMP templateText
                                      ss (Galaxy xs) = xs
                                  in  toString $ setAttrs template (ss g) 

setAttrs template ss =  setAttribute "factionName" Config.factionName 
                      $ setAttribute "systemsTotal"  (systemsTotal ss)
                      $ setAttribute "populatedSystemsTotal" (populatedSystemsTotal ss)
                      $ template

systemsTotal galaxy = show (length galaxy)

populatedSystemsTotal galaxy = show (length $ populatedSystems galaxy)
              
