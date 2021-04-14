-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Helpers to implement pagination.

module Edna.Web.Pagination
  ( noPageHeaders
  , getPaginatedHelper
  , moreThanOnePagination
  ) where

import Universum

import Servant (ServerError(..), err400)
import Servant.API (Headers, ToHttpApiData, noHeader)
import Servant.Pagination (AcceptRanges, PageHeaders)

import Edna.Setup (Edna)

-- | @servant-pagination@ defines a number of headers for paginated responses.
-- However, if no pagination header was provided, we just return all items and
-- do not include any headers. This function creates a suitable value without headers.
noPageHeaders ::
  ToHttpApiData (AcceptRanges fields) =>
  res -> Headers (PageHeaders fields a) res
noPageHeaders = noHeader . noHeader . noHeader

-- | A helper function that elimiates a small portion of boilerplate in
-- implemenation of paginated getters.
getPaginatedHelper ::
  ( ToHttpApiData (AcceptRanges fields)
  , response ~ (Headers (PageHeaders fields resource) resources)
  ) =>
  Maybe ranges -> Edna resources -> (ranges -> resources -> Edna response) -> Edna response
getPaginatedHelper mRange getAll callback = case mRange of
  Nothing -> noPageHeaders <$> getAll
  Just range -> callback range =<< getAll

-- | It's not clear how to handle the case when more than one @Range@ is provided
-- because @returnRange@ needs only one. Moreover, it's probably just not possible.
-- We throw this error if such case happens.
moreThanOnePagination :: ServerError
moreThanOnePagination = err400
  { errBody = "more than one pagination range is provided, it's not supported"}
