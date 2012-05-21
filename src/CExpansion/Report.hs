module CExpansion.Report ( printFactionInfo ) where

import Text.StringTemplate
import CExpansion.Galaxy
import qualified Config ( factionName )

printFactionInfo templateText g notifications = 
                                  let template = newSTMP templateText
                                      ss (Galaxy xs) = xs
                                  in  toString $ setAttrs template (ss g) notifications

setAttrs template ss notifications = 
                        setAttribute "notifications" notifications 
                      $ setAttribute "factionName" Config.factionName 
                      $ setAttribute "systemsTotal"  (systemsTotal ss)
                      $ setAttribute "populatedSystemsTotal" (populatedSystemsTotal ss)
                      $ setAttribute "populationTotal" (populationTotal ss)
                      $ template

systemsTotal galaxy = show (length galaxy)

populatedSystemsTotal galaxy = show (length $ populatedSystemsForGalaxy galaxy)

populationTotal galaxy = show $ sum $ map population $ humanDetailsForGalaxy galaxy
