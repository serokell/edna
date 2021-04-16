-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Test.API.Util
  ( app
  , clientEnv
  , apiTry
  , errorWithStatus
  ) where

import Universum

import Control.Exception (throwIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status)
import Servant (Application, serve)
import Servant.Client
  (BaseUrl(baseUrlPort), ClientEnv, ClientError(..), ClientM, mkClientEnv, parseBaseUrl,
  responseStatusCode, runClientM)

import Edna.Setup (EdnaContext)
import Edna.Web.API (ednaAPI)
import Edna.Web.Server (ednaServer)

app :: EdnaContext -> IO Application
app ctx = return $ serve ednaAPI $ ednaServer ctx

-- Creating a new Manager is a relatively expensive operation, so we will
-- share a single Manager between tests
clientEnv :: Int -> IO ClientEnv
clientEnv port = do
  baseUrl <- parseBaseUrl "http://localhost"
  manager <- newManager defaultManagerSettings
  return $ mkClientEnv manager (baseUrl { baseUrlPort = port })

apiTry :: ClientEnv -> ClientM a -> IO a
apiTry env action = either throwIO return =<< runClientM action env

errorWithStatus :: Status -> ClientError -> Bool
errorWithStatus status servantError = case servantError of
  FailureResponse _ response -> responseStatusCode response == status
  _ -> False
