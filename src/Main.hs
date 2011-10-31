{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module Main ( main ) where

import Control.Applicative  ( (<$>))
import Control.Exception    ( bracket)
import Control.Monad        ( msum)
import Control.Monad.Reader ( ask)
import Control.Monad.State  ( get, put)
import Happstack.Server
import Happstack.State
import CExpansion.Galaxy
import CExpansion.Report ( printFactionInfo )

turn = id

execTurn :: Update AppState Galaxy
execTurn =
    do appState <- get
       let new = turn (galaxy appState) 
       put $ appState { galaxy = new } 
       return new

getGalaxy :: Query AppState Galaxy
getGalaxy = galaxy <$> ask

$(mkMethods ''AppState ['execTurn, 'getGalaxy])

handlers :: ServerPart Response
handlers = msum [ dir "report" $ do g <- query GetGalaxy
                                    ok $ toResponse $ printFactionInfo g
                , do nullDir 
                     c <- update ExecTurn
                     ok $ toResponse $ "Home" 
                ]

main = do bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $ 
            \_control -> simpleHTTP nullConf {port=80} handlers
       where createCheckpointAndShutdown control = do createCheckpoint control
                                                      shutdownSystem control


