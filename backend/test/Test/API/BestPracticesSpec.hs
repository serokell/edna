-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | API testing for best practices

module Test.API.BestPracticesSpec
  ( spec
  ) where

import Universum

import Network.HTTP.Client (httpLbs, method, responseBody, responseStatus)
import Network.HTTP.Types (methodDelete, status204, status404, statusIsSuccessful)
import Servant.QuickCheck (Predicates, RequestPredicate(..), not500, (<%>))
import Servant.QuickCheck.Internal (PredicateFailure(..))
import Test.Hspec (Spec, beforeAllWith, describe, it)

import Test.API.Util (propertyTestRunner)
import Test.Setup (runInit, specWithContext)

spec :: Spec
spec = specWithContext $ do
  beforeAllWith (\ctx -> ctx <$ runInit ctx) $ do
    describe "API property testing" $ do
      it "demonstrates best practices" $
        propertyTestRunner True predicates

-- | All the predicates we want to enforce in our API.
predicates :: Predicates
predicates =
      not500
  <%> deleteReqShouldReturn204
  <%> noEmptyBody
  <%> mempty

--
-- RESTful-abiding predicates
--

-- | Checks that every DELETE request should return a 204 NoContent.
deleteReqShouldReturn204 :: RequestPredicate
deleteReqShouldReturn204 = RequestPredicate $ \req mgr -> do
  if method req == methodDelete
  then do
    resp <- httpLbs req mgr
    let status = responseStatus resp
    when (statusIsSuccessful status && status /= status204) $
      throwM $ PredicateFailure "deleteReqShouldReturn204" (Just req) resp
    return [resp]
  else return []

-- | Checks that every request which is not a 204 No Content
-- does not have an empty body, but it always returns something.
noEmptyBody :: RequestPredicate
noEmptyBody = RequestPredicate $ \req mgr -> do
  resp <- httpLbs req mgr
  let body   = responseBody resp
  let status = responseStatus resp
  when (status `notElem` [status204, status404] && body == mempty) $
    throwM $ PredicateFailure "noEmptyBody" (Just req) resp
  return [resp]
