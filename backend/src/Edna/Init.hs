-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Edna initialization shared by code that runs 'Edna' actions.

module Edna.Init
  ( initEdna
  ) where

import Universum

import RIO (BufferMode(LineBuffering), hSetBuffering)

import Edna.Analysis.FourPL (check4PLConfiguration)
import Edna.DB.Initialisation (schemaInit)
import Edna.Setup (Edna)

-- | Actions that are typically performed before running some 'Edna' action:
--
-- * Setup logging.
-- * Check Python environment.
-- * Initialize DB.
initEdna :: Edna ()
initEdna = do
    -- We print logs to stderr and LineBuffering is most appropriate for logging.
  hSetBuffering stderr LineBuffering
  check4PLConfiguration
  schemaInit
