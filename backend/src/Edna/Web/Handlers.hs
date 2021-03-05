{-# LANGUAGE StarIsType #-}

module Edna.Web.Handlers
  ( ednaHandlers
  , EdnaServerError (..)
  )
where

import Universum

import Servant.API.Generic (ToServant)
import Servant.Multipart (FileData(..), Mem, MultipartData(..))
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.Web.API (EdnaEndpoints(..))
import Edna.Web.Error (EdnaServerError(..))
import Edna.Web.Types (ExperimentalMeasurement(..))

type EdnaHandlers m = ToServant EdnaEndpoints (AsServerT m)

-- | Server handler implementation for Edna API.
ednaHandlers :: EdnaHandlers Handler
ednaHandlers = genericServerT EdnaEndpoints
  { eeUploadExperiment = uploadExperiment
  }

uploadExperiment :: MultipartData Mem -> Handler [ExperimentalMeasurement]
uploadExperiment multipart = do
  file <- maybe (throwM NoExperimentFileError) pure (safeHead (files multipart))
  putStrLn $ "Excel file name " ++ show (fdFileName file)
  either (throwM . XlsxParingError) pure (parseExperimentXls $ fdPayload file)
