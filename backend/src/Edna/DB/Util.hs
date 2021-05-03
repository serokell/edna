-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Utility module to write functions that deal with DB.

module Edna.DB.Util
  ( groupAndPaginate
  , sortingSpecWithId
  ) where

import Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set

import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Control.Lens (at)
import Servant.Util (PaginationSpec(..), SortingSpec, TyNamedParam, type (?:))
import Servant.Util.Combinators.Sorting.Base (SortingItem(..), SortingOrder(..), SortingSpec(..))
import Servant.Util.Dummy.Pagination (paginate)

import Edna.DB.Schema ()

-- | This function is helpful for queries where we use left join and get
-- pairs of items. The first item is the main and legendary one, it is requested.
-- We call it @boka@. Each @boka@ has a list of grandchildren, we call them
-- @joka@. There can be multiple items with the same @boka@ values. @boka@
-- has a primary key that we use for identification.
-- There are 3 goals here:
--
-- 1. Group items with the same @boka@ value.
-- 2. Preserving sorting of items.
-- 3. Carefully apply pagination, it should be applied __after__ grouping.
groupAndPaginate :: forall boka joka pk.
  (Hashable pk, Eq pk, Ord joka) =>
  Maybe PaginationSpec -> (boka -> pk) ->
  [(boka, Maybe joka)] -> [(boka, [joka])]
groupAndPaginate mPagination toPrimaryKey items =
  maybe id paginate mPagination $ snd $ foldr (step . fst) (mempty, []) items
  where
    step :: boka -> (HashSet pk, [(boka, [joka])]) -> (HashSet pk, [(boka, [joka])])
    step boka acc@(visited, res)
      | HashSet.member pk visited = acc
      | otherwise =
        ( HashSet.insert pk visited
        , (boka, maybe [] toList $ bokaToJokas ^. at pk) : res
        )
      where
        pk = toPrimaryKey boka

    bokaToJokas :: HashMap pk (Set joka)
    bokaToJokas = foldl' innerStep mempty items
      where
        innerStep acc (boka, mJoka) =
          maybe acc (\joka -> HM.insertWith Set.union (toPrimaryKey boka) (one joka) acc) mJoka

-- | Add implicit sorting order by descending ID. It ensures we always have a
-- predictable sorting order for items with equal sorting keys or when
-- no explicit sorting order is provided.
sortingSpecWithId :: forall (params :: [TyNamedParam Type]).
  SortingSpec params -> SortingSpec (("id" ?: SqlSerial Word32) ': params)
sortingSpecWithId = SortingSpec . (<> [sortingItem]) . unSortingSpec
  where
    sortingItem = SortingItem "id" Descendant
