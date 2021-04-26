// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { UsePaginationOptions, UseSortByOptions } from "react-table";

// Taken from here: https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/react-table
declare module "react-table" {
  export interface TableOptions<D extends Record<string, unknown>>
    extends UsePaginationOptions<D>,
      UseSortByOptions<D> {}

  export type Hooks<D extends Record<string, unknown> = Record<string, unknown>> = UseSortByHooks<
    D
  >;

  export interface TableInstance<D extends Record<string, unknown> = Record<string, unknown>>
    extends UsePaginationInstanceProps<D>,
      UseSortByInstanceProps<D> {}

  export interface TableState<D extends Record<string, unknown> = Record<string, unknown>>
    extends UsePaginationState<D>,
      UseSortByState<D> {}

  export type ColumnInterface<
    D extends Record<string, unknown> = Record<string, unknown>
  > = UseSortByColumnOptions<D>;

  export interface ColumnInstance<D extends Record<string, unknown> = Record<string, unknown>>
    extends UseResizeColumnsColumnProps<D>,
      UseSortByColumnProps<D> {}
}
