{-# LANGUAGE OverloadedLabels #-}

{-|
Utilities for running Edna web server.
-}
module Edna.Web.Server
       ( runEdna
       , serveWeb
       , addrSettings
       ) where

import Universum

import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, Handler, Server, hoistServer, serve, throwError)

import Edna.Config
import Edna.Util
import Edna.Web.API
import Edna.Web.Handlers
import Edna.Web.Error (toServerError)

-- | Sets the given listen address in a Warp server settings.
addrSettings :: NetworkAddress -> Warp.Settings
addrSettings NetworkAddress {..} = Warp.defaultSettings
  & Warp.setHost (fromString $ toString naHost)
  & Warp.setPort (fromIntegral naPort)

-- | Helper for running a Warp server on a given listen port in
-- arbitrary @MonadIO@.
serveWeb :: MonadIO m => NetworkAddress -> Application -> m a
serveWeb addr app = do
  liftIO $ Warp.runSettings (addrSettings addr) app
  return $ error "Server terminated early"

-- | Makes the @Server@ for Edna API, given the natural
-- transformation from the working monad to @Handler@.
ednaServer :: Server EdnaAPI
ednaServer = hoistServer ednaAPI translateExceptions ednaHandlers

-- | Translate exceptions to http responses with corresponding statuses
translateExceptions :: Handler a -> Handler a
translateExceptions action =
  action
  `catch` throwServant
  where
    throwServant = throwError . toServerError @EdnaServerError

-- | Runs the web server which serves Edna API.
runEdna :: EdnaConfig -> IO ()
runEdna EdnaConfig{..} = do
  let listenAddr = acListenAddr ecApi

  serveWeb listenAddr $
    serve ednaAPI ednaServer
