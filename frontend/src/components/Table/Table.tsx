// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { useState } from "react";
import { Column, useTable } from "react-table";
import { v4 as uuidv4 } from "uuid";
import "./Table.scss";
import cn from "../../utils/bemUtils";

interface ColumnExtra {
  manualCellRendering?: boolean;
}

// eslint-disable-next-line @typescript-eslint/ban-types
interface LibraryTableProps<T extends object> {
  data: T[];
  columns: Column<T>[];
  columnExtras?: { [id: string]: ColumnExtra };
  className?: string;
  small?: boolean;
  collapsible?: (x: T) => React.ReactNode;
}

// eslint-disable-next-line @typescript-eslint/ban-types
export function Table<T extends object>({
  data,
  columns,
  columnExtras,
  className,
  small,
  collapsible,
}: LibraryTableProps<T>): React.ReactElement {
  const { getTableProps, getTableBodyProps, headerGroups, rows, prepareRow } = useTable({
    columns,
    data,
  });
  const isCollapsible = !!collapsible;
  const lastColumnWithRightBorder = computeLastColumnWithRightBorder(columns);
  const ednaTable = cn("ednaTable");
  const [shownColl, setShownColl] = useState<boolean[]>(new Array(data.length).fill(false));

  return (
    <table {...getTableProps()} className={`ednaTable ${className ?? ""}`}>
      <thead>
        {headerGroups.map(headerGroup => (
          <tr {...headerGroup.getHeaderGroupProps()}>
            {headerGroup.headers.map((column, i) => {
              return (
                <th {...column.getHeaderProps()} className="ednaTable__columnHead">
                  {column.render("Header")}
                  {i <= lastColumnWithRightBorder && (
                    <span className="ednaTable__headRightBorder" />
                  )}
                </th>
              );
            })}
          </tr>
        ))}
      </thead>
      <tbody {...getTableBodyProps()}>
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
                    typeof classNm === "string" &&
                    classNm.indexOf("ednaTable__cell") !== -1
                  ) {
                    const newShownColl = shownColl.slice();
                    newShownColl[i] = !shownColl[i];
                    setShownColl(newShownColl);
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
              {collapsible && shownColl[i] && (
                <tr>
                  <td
                    colSpan={columns.length}
                    className={ednaTable("cellShownCollapse", { shown: shownColl[i] })}
                  >
                    {collapsible(row.original)}
                  </td>
                </tr>
              )}
            </React.Fragment>
          );
        })}
      </tbody>
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
