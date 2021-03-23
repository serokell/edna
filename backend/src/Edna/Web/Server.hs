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
import RIO (runRIO)
import Servant (Application, Handler, Server, hoistServer, serve, throwError)

import Edna.Config.Definition (acListenAddr, acServeDocs, ecApi)
import Edna.Config.Utils (fromConfig)
import Edna.DB.Initialisation (schemaInit)
import Edna.ExperimentReader.Error (ExperimentParsingError)
import Edna.Library.Error (LibraryError)
import Edna.Setup (Edna, EdnaContext)
import Edna.Upload.Error (UploadApiError, UploadError)
import Edna.Util (NetworkAddress(..))
import Edna.Web.API (EdnaAPI, ednaAPI)
import Edna.Web.Error (ToServerError(..))
import Edna.Web.Handlers (ednaHandlers)
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

-- | Makes the @Server@ for Edna API, given 'EdnaContext'.
ednaServer :: EdnaContext -> Server EdnaAPI
ednaServer ctx = hoistServer ednaAPI (ednaToHandler ctx) ednaHandlers

-- | Run 'Edna' action inside 'Handler' monad.
--
-- * Translate exceptions to http responses with corresponding statuses.
-- * Provide 'EdnaContext'.
ednaToHandler :: EdnaContext -> Edna a -> Handler a
ednaToHandler ctx action =
  runRIO ctx action
  `catch` throwPurely @ExperimentParsingError
  `catch` throwPurely @UploadError
  `catch` throwPurely @UploadApiError
  `catch` throwPurely @LibraryError
  `catch` throwError   -- catch 'ServantError'
  where
    throwPurely :: ToServerError e => e -> Handler a
    throwPurely = throwError . toServerError

-- | Runs the web server which serves Edna API.
edna :: Edna ()
edna = do
  schemaInit
  listenAddr <- fromConfig $ ecApi . acListenAddr
  withDocs <- fromConfig $ ecApi . acServeDocs
  server <- ednaServer <$> ask
  serveWeb listenAddr
    if withDocs then
      serve ednaAPIWithDocs (withSwaggerUI ednaAPI ednaApiSwagger server)
    else serve ednaAPI server
