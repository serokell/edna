-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Logging primitives.

module Edna.Logging
  ( logDebug
  , logMessage
  , logUnconditionally
  ) where

import Universum

import Edna.Config.Definition (LoggingConfig(..), ecLogging)
import Edna.Config.Utils (fromConfig)
import Edna.Setup (Edna)
import Edna.Util (logUnconditionally)

-- | Log a debug message with low severity, only in development logging mode.
logDebug :: Text -> Edna ()
logDebug = logImpl (== LogDev)

-- | Log a really useful message, unless logging is disabled.
logMessage :: Text -> Edna ()
logMessage = logImpl (/= LogNothing)

logImpl :: (LoggingConfig -> Bool) -> Text -> Edna ()
logImpl cond msg = do
  lc <- fromConfig ecLogging
  when (cond lc) $ logUnconditionally msg
