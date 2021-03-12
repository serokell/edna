module Edna.Web.Handlers
  ( ednaHandlers
  , EdnaServerError (..)
  )
where

import Universum

import Servant.API.Generic (ToServant)
import Servant.Multipart (FileData(..), Mem, MultipartData(..))
import Servant.Server (Handler, err501)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.Web.API (EdnaEndpoints(..), MethodologyEndpoints(..), ProjectEndpoints(..), CompoundEndpoints (..))
import Edna.Web.Error (EdnaServerError(..))
import Edna.Web.Types (ExperimentalMeasurement(..))

type EdnaHandlers m = ToServant EdnaEndpoints (AsServerT m)

-- | Server handler implementation for Edna API.
ednaHandlers :: EdnaHandlers Handler
ednaHandlers = genericServerT EdnaEndpoints
  { eeUploadExperiment = uploadExperiment
  , eeProjectEndpoints = projectEndpoints
  , eeMethodologyEndpoints = methodologyEndpoints
  , eeCompoundEndpoints = compoundEndpoints
  }
  where
    projectEndpoints = genericServerT ProjectEndpoints
      { peAddProject = \_ -> throwM err501
      , peEditProject = \_ _ -> throwM err501
      , peGetProjects = \_ _ _ -> throwM err501
      , peGetProject = \_ -> throwM err501
      }

    methodologyEndpoints = genericServerT MethodologyEndpoints
      { meAddMethodology = \_ -> throwM err501
      , meEditMethodology = \_ _ -> throwM err501
      , meGetMethodologies = \_ _ _ -> throwM err501
      , meGetMethodology = \_ -> throwM err501
      }

    compoundEndpoints = genericServerT CompoundEndpoints
      { ceEditChemSoft = \_ _ -> throwM err501
      , ceGetCompounds = \_ _ _ -> throwM err501
      , ceGetCompound = \_ -> throwM err501
      }

uploadExperiment :: MultipartData Mem -> Handler [ExperimentalMeasurement]
uploadExperiment multipart = do
  file <- maybe (throwM NoExperimentFileError) pure (safeHead (files multipart))
  putStrLn $ "Excel file name " ++ show (fdFileName file)
  either (throwM . XlsxParingError) pure (parseExperimentXls $ fdPayload file)
