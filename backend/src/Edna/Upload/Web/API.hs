-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Upload-related part of API definition along with implementation.

module Edna.Upload.Web.API
  ( FileUploadEndpoints (..)
  , FileUploadAPI
  , fileUploadEndpoints
  ) where

import Universum

import Servant.API (JSON, Post, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Multipart (Mem, MultipartForm)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.Setup (Edna)
import Edna.Upload.Service (parseFile, uploadFile)
import Edna.Upload.Web.Types (FileBS(..), FileSummary, FileUploadReq(..))

-- | Endpoints necessary to implement file uploading.
data FileUploadEndpoints route = FileUploadEndpoints
  { -- | Parse the file and return its summary for preview.
    -- It doesn't change any state, but it's POST because GET can't
    -- receive multipart.
    fueParseFile :: route
      :- "parse"
      :> Summary "Parse the file and return its summary for preview"
      :> MultipartForm Mem FileBS
      :> Post '[JSON] FileSummary

  , -- | Upload the file with some methodology and project.
    fueUploadFile :: route
      :- "upload"
      :> Summary "Upload the file with some methodology and project"
      :> MultipartForm Mem FileUploadReq
      :> Post '[JSON] FileSummary
  } deriving stock (Generic)

type FileUploadAPI = ToServant FileUploadEndpoints AsApi

fileUploadEndpoints :: ToServant FileUploadEndpoints (AsServerT Edna)
fileUploadEndpoints = genericServerT FileUploadEndpoints
  { fueParseFile = parseFile . fbsFile
  , fueUploadFile = \FileUploadReq{furFile = FileBS{..}, ..} ->
      uploadFile furProject furTestMethodology (fromMaybe "" furDescription) fbsName fbsFile
  }
