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
import Text.StringTemplate
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.UTF8       as LU (fromString)
import Config
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

handlers :: [String] -> ServerPart Response
handlers templates = uriRest $ action (templates !! 0)

action template "/turn" = do g <- update ExecTurn
                             ok $ toResponse $ Page $ printFactionInfo template g 
action template _       = do g <- query GetGalaxy 
                             ok $ toResponse $ Page $ printFactionInfo template g

main = do templates <- loadTemplates
          bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $ 
            \_control -> simpleHTTP nullConf {port=serverPort} (handlers templates)
       where createCheckpointAndShutdown control = do createCheckpoint control
                                                      shutdownSystem control

loadTemplates = do templateText <- readFile "tpl/report.st"
                   return [templateText]

data Page = Page String

instance ToMessage Page where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage (Page s) = LU.fromString s

