{-|
Utilities for running Edna web server.
-}
module Edna.Web.Server
  ( edna
  , serveWeb
  , addrSettings
  ) where

import Universum

import Data.Default (def)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
  (Destination(..), IPAddrSource(..), OutputFormat(..), RequestLoggerSettings(..), mkRequestLogger)
import RIO (runRIO)
import Servant
  (Application, Handler, NoContent(..), Server, hoistServer, serve, throwError, (:<|>)(..))
import Servant.Util.Combinators.Logging (ServantLogConfig(..), serverWithLogging)

import Edna.Analysis.FourPL (check4PLConfiguration)
import Edna.Config.Definition (LoggingConfig(..), acListenAddr, acServeDocs, ecApi, ecLogging)
import Edna.Config.Utils (fromConfig)
import Edna.DB.Initialisation (schemaInit)
import Edna.Dashboard.Error (DashboardError)
import Edna.ExperimentReader.Error (ExperimentParsingError)
import Edna.Library.Error (LibraryError)
import Edna.Orphans ()
import Edna.Setup (Edna, EdnaContext)
import Edna.Upload.Error (UploadApiError, UploadError)
import Edna.Util (NetworkAddress(..))
import Edna.Web.API (EdnaAPI, ednaAPI)
import Edna.Web.Error (ToServerError(..))
import Edna.Web.Handlers (ednaHandlers)
import Edna.Web.Swagger (EdnaAPIWithDocs, ednaAPIWithDocs, ednaApiSwagger, withSwaggerUI)

-- | Sets the given listen address in a Warp server settings.
addrSettings :: NetworkAddress -> Warp.Settings
addrSettings NetworkAddress {..} = Warp.defaultSettings
  & Warp.setHost (fromString $ toString naHost)
  & Warp.setPort (fromIntegral naPort)

-- | Helper for running a Warp server on a given listen port in
-- arbitrary @MonadIO@.
serveWeb :: MonadIO m => NetworkAddress -> LoggingConfig -> Application -> m a
serveWeb addr loggingConfig app = liftIO $ do
  middleware <- fromMaybe id <$> loggingMiddleware
  Warp.runSettings (addrSettings addr) $ middleware app
  return $ error "Server terminated early"
  where
    -- This function creates a non-verbose logging middleware if logging mode
    -- is 'LogProd'.
    loggingMiddleware :: IO (Maybe Middleware)
    loggingMiddleware = runMaybeT $ do
      settings <- case loggingConfig of
        LogProd -> pure $ def { outputFormat = Apache FromSocket }
        _ -> empty

      lift $ mkRequestLogger $ settings { destination = Handle stderr }

-- | Makes the @Server@ for Edna API, given 'EdnaContext'.
ednaServer :: EdnaContext -> Server EdnaAPI
ednaServer ctx =
  hoistServer ednaAPI (ednaToHandler ctx) (ednaHandlers :<|> pure NoContent)

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
  `catch` throwPurely @DashboardError
  `catch` throwError   -- catch 'ServantError'
  where
    throwPurely :: ToServerError e => e -> Handler a
    throwPurely = throwError . toServerError

-- | Runs the web server which serves Edna API.
edna :: Edna ()
edna = do
  check4PLConfiguration
  schemaInit
  listenAddr <- fromConfig $ ecApi . acListenAddr
  withDocs <- fromConfig $ ecApi . acServeDocs
  loggingConfig <- fromConfig ecLogging
  server <- ednaServer <$> ask
  let
    servantLogConfig :: ServantLogConfig
    servantLogConfig = ServantLogConfig (hPutStrLn stderr)

    serverWithDocs :: Server EdnaAPIWithDocs
    serverWithDocs = withSwaggerUI ednaAPI ednaApiSwagger server

    app :: Application
    app
      -- Dev logging and docs
      | LogDev <- loggingConfig
      , withDocs =
        serverWithLogging servantLogConfig ednaAPIWithDocs $
          \api -> serve api serverWithDocs
      -- No dev logging, docs
      | withDocs = serve ednaAPIWithDocs serverWithDocs
      -- Dev logging, no docs
      | LogDev <- loggingConfig =
        serverWithLogging servantLogConfig ednaAPI $
          \api -> serve api server
      -- No dev logging, no docs
      | otherwise = serve ednaAPI server

  serveWeb listenAddr loggingConfig app
