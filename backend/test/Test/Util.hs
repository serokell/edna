-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Utilities used only in tests.

module Test.Util
  ( DefaultPgNullsOrder (..)
  , methodologyReqToResp
  ) where

import Universum

import Edna.Library.Web.Types (MethodologyReq(..), MethodologyResp(..))

-- | According to <https://www.postgresql.org/docs/current/sql-select.html>:
-- the default is to act as though nulls are larger than non-nulls.
-- Ideally we should pass @NULLS FIRST@ or @NULLS LAST@ explicitly, but it's not
-- supported by @servant-util@ yet.
-- This newtype changes the 'Ord' instance for anything wrapped into 'Maybe' to
-- treat 'Nothing' values greater than 'Just'.
newtype DefaultPgNullsOrder a = DefaultPgNullsOrder (Maybe a)
  deriving stock (Eq)

instance Ord a => Ord (DefaultPgNullsOrder a) where
  DefaultPgNullsOrder a <= DefaultPgNullsOrder b = case (a, b) of
    (_, Nothing) -> True
    (Nothing, Just _) -> False
    (Just a', Just b') -> a' <= b'

-- | Created 'MethodologyResp' from 'MethodologyReq' with a list of project names.
methodologyReqToResp :: MethodologyReq -> [Text] -> MethodologyResp
methodologyReqToResp MethodologyReq {..} projects = MethodologyResp
  { mrName = mrqName
  , mrDescription = mrqDescription
  , mrConfluence = mrqConfluence
  , mrProjects = projects
  }
