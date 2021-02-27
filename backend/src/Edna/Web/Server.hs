{-|
Utilities for running Edna web server.
-}
module Edna.Web.Server
  ( edna
  , serveWeb
  , addrSettings
  ) where

import Universum

import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, Handler, Server, hoistServer, serve, throwError)

import Edna.Config.Definition (acListenAddr, acServeDocs, ecApi)
import Edna.Config.Utils (fromConfig)
import Edna.DB.Integration (runPg, transact)
import Edna.DB.Schema (ensureSchemaIsSetUp)
import Edna.Setup (Edna)
import Edna.Util (NetworkAddress(..))
import Edna.Web.API (EdnaAPI, ednaAPI)
import Edna.Web.Error (toServerError)
import Edna.Web.Handlers (EdnaServerError, ednaHandlers)
import Edna.Web.Swagger (ednaAPIWithDocs, ednaApiSwagger, withSwaggerUI)

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
edna :: Edna ()
edna = do
  transact (runPg ensureSchemaIsSetUp)
  listenAddr <- fromConfig $ ecApi . acListenAddr
  withDocs <- fromConfig $ ecApi . acServeDocs
  serveWeb listenAddr $
    if withDocs then
      serve ednaAPIWithDocs (withSwaggerUI ednaAPI ednaApiSwagger ednaServer)
    else serve ednaAPI ednaServer
