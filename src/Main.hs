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
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.UTF8       as LU (fromString)
import Config
import CExpansion.Galaxy
import CExpansion.Report ( printFactionInfo )
import CExpansion.Turn ( turn )
import CExpansion.Init ( initGalaxy )

execInit :: Update AppState Galaxy
execInit = do rs <- randoms (1000 * Config.galaxyInitSystemsNumber) -- XXX fix randoms calculation
              let new = initGalaxy rs
              appState <- get
              put $ appState { galaxy = new }
              return new

randoms 0 = return []
randoms n = do x <- getRandom
               rs <- randoms (n-1)
               return (x:rs)

execTurn :: Update AppState Galaxy
execTurn =
    do appState <- get
       let new = turn (galaxy appState) 
       put $ appState { galaxy = new } 
       return new

getGalaxy :: Query AppState Galaxy
getGalaxy = galaxy <$> ask

$(mkMethods ''AppState ['execInit, 'execTurn, 'getGalaxy])

handlers :: [String] -> ServerPart Response
handlers templates = uriRest $ action (templates !! 0)

action _   "/dump" = do g <- query GetGalaxy
                        dumpResponse g
action tpl "/init" = do g <- update ExecInit
                        reportResponse tpl g "Galaxy has been initialized"
action tpl "/turn" = do g <- update ExecTurn
                        reportResponse tpl g "Turn!!"
action tpl _       = do g <- query GetGalaxy 
                        reportResponse tpl g ""

dumpResponse galaxy = ok $ toResponse $ PlainText $ show galaxy

reportResponse template galaxy notifications = ok $ toResponse $ Page $ printFactionInfo template galaxy notifications

main = do templates <- loadTemplates
          bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $ 
            \_control -> simpleHTTP nullConf {port = Config.serverPort} (handlers templates)
       where createCheckpointAndShutdown control = do createCheckpoint control
                                                      shutdownSystem control

loadTemplates = do templateText <- readFile "tpl/report.st"
                   return [templateText]

data Page = Page String
data PlainText = PlainText String

instance ToMessage Page where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage (Page s) = LU.fromString s

instance ToMessage PlainText where
    toContentType _ = B.pack "text/plain; charset=UTF-8"
    toMessage (PlainText s) = LU.fromString s

