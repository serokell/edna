// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { useEffect, useState } from "react";
import { Column, useSortBy, useTable } from "react-table";
import { v4 as uuidv4 } from "uuid";
import "./Table.scss";
import { isRecoilValue, RecoilValueReadOnly, useRecoilValueLoadable } from "recoil";
import cn from "../../utils/bemUtils";
import ArrowSvg from "../../assets/svg/arrow.svg";
import { Spinner } from "../Spinner/Spinner";
import { SortParamsApi } from "../../api/EdnaApi";

interface ColumnExtra {
  manualCellRendering?: boolean;
}

// eslint-disable-next-line @typescript-eslint/ban-types
interface TableProps<T extends object> {
  columns: Column<T>[];
  columnExtras?: { [id: string]: ColumnExtra };
  headerExtraStyles?: { [id: string]: string };
  className?: string;
  small?: boolean;
  collapsible?: (x: T) => React.ReactNode;
  collapsibleId?: (x: T) => number;
  defaultSortedColumn: string;
  dataOrQuery:
    | RecoilValueReadOnly<ReadonlyArray<T>>
    | ((param: SortParamsApi) => RecoilValueReadOnly<T[]>);
}

// eslint-disable-next-line @typescript-eslint/ban-types
export function Table<T extends object>({
  columns,
  columnExtras,
  headerExtraStyles,
  className,
  small,
  collapsible,
  collapsibleId,
  dataOrQuery,
  defaultSortedColumn,
}: TableProps<T>): React.ReactElement {
  const isConstant = isRecoilValue(dataOrQuery);
  const [data, setData] = useState<T[]>([]);

  const {
    getTableProps,
    getTableBodyProps,
    headerGroups,
    rows,
    prepareRow,
    state: { sortBy },
  } = useTable<T>(
    {
      columns,
      data,
      initialState: {
        sortBy: [
          {
            id: defaultSortedColumn,
          },
        ],
      },
      disableMultiSort: true,
      manualSortBy: !isConstant,
      disableSortRemove: true,
    },
    useSortBy
  );

  const loadableData = useRecoilValueLoadable(
    typeof dataOrQuery === "function"
      ? dataOrQuery(sortBy.length === 0 ? {} : { sortby: sortBy[0].id, desc: sortBy[0].desc })
      : dataOrQuery
  );

  useEffect(() => {
    if (loadableData.state === "hasValue") {
      setData(loadableData.contents.concat([]));
    } else {
      setData([]);
    }
  }, [loadableData]);

  const isCollapsible = !!collapsible;
  const lastColumnWithRightBorder = computeLastColumnWithRightBorder(columns);
  const ednaTable = cn("ednaTable");
  const [shownColl, setShownColl] = useState<Set<number>>(new Set());

  function renderRows(): React.ReactElement {
    if (loadableData.state === "loading") {
      return (
        <tr>
          <td colSpan={columns.length}>
            <Spinner />
          </td>
        </tr>
      );
    }
    if (loadableData.state === "hasError") {
      return (
        <tr>
          <td colSpan={columns.length} className="ednaTable__error">
            {loadableData.contents.message}
          </td>
        </tr>
      );
    }

    return (
      <>
        {rows.map((row, i) => {
          prepareRow(row);
          return (
            <React.Fragment key={uuidv4()}>
              <tr
                {...row.getRowProps()}
                className={ednaTable("row", { collapsible: isCollapsible, striped: i % 2 === 0 })}
                onClick={e => {
                  const classNm = (e.target as any).className;
                  if (
                    isCollapsible &&
                    collapsibleId &&
                    typeof classNm === "string" &&
                    classNm.indexOf("ednaTable__cell") !== -1
                  ) {
                    setShownColl(prev => {
                      const cId = collapsibleId(row.original);
                      if (prev.has(cId)) {
                        prev.delete(cId);
                        return new Set(prev);
                      }
                      return new Set(prev.add(cId));
                    });
                  }
                }}
              >
                {row.cells.map(cell => {
                  const manualCell =
                    cell.column.id &&
                    columnExtras &&
                    columnExtras[cell.column.id] &&
                    columnExtras[cell.column.id].manualCellRendering;
                  return manualCell ? (
                    cell.render("Cell", cell.getCellProps())
                  ) : (
                    <td {...cell.getCellProps()} className={ednaTable("cell", { small })}>
                      {cell.render("Cell")}
                    </td>
                  );
                })}
              </tr>
              {collapsible && collapsibleId && shownColl.has(collapsibleId(row.original)) && (
                <tr>
                  <td
                    colSpan={columns.length}
                    className={ednaTable("cellShownCollapse", {
                      shown: shownColl.has(collapsibleId(row.original)),
                    })}
                  >
                    {collapsible(row.original)}
                  </td>
                </tr>
              )}
            </React.Fragment>
          );
        })}
      </>
    );
  }

  return (
    <table {...getTableProps()} className={`ednaTable ${className ?? ""}`}>
      <thead>
        {headerGroups.map(headerGroup => (
          <tr {...headerGroup.getHeaderGroupProps()}>
            {headerGroup.headers.map((column, i) => {
              return (
                <th
                  {...column.getHeaderProps(column.getSortByToggleProps())}
                  className={`ednaTable__columnHead ${
                    headerExtraStyles ? headerExtraStyles[column.id] : ""
                  }`}
                  title={column.canSort ? `Sort by ${column.Header}` : ""}
                >
                  {column.render("Header")}
                  {column.canSort && (
                    <span
                      className={ednaTable("sortSign", {
                        desc: column.isSortedDesc,
                        vis: column.isSorted,
                      })}
                    >
                      <ArrowSvg />
                    </span>
                  )}
                  {i <= lastColumnWithRightBorder && (
                    <span className="ednaTable__headRightBorder" />
                  )}
                </th>
              );
            })}
          </tr>
        ))}
      </thead>
      <tbody {...getTableBodyProps()}>{renderRows()}</tbody>
    </table>
  );
}

// eslint-disable-next-line @typescript-eslint/ban-types
function computeLastColumnWithRightBorder<T extends object>(columns: Column<T>[]) {
  let lastColumnWithRightBorder = columns.length - 2;
  for (let i = columns.length - 1; i >= 0; i--) {
    lastColumnWithRightBorder = i - 1;
    if (columns[i].Header) {
      break;
    }
  }
  return lastColumnWithRightBorder;
}
