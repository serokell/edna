import React from "react";
import { Column, useTable } from "react-table";
import "./Table.scss";
import cx from "classnames";

// eslint-disable-next-line @typescript-eslint/ban-types
interface LibraryTableProps<T extends object> {
  data: T[];
  columns: Column<T>[];
  className?: string;
  mode: "bordered" | "alternate";
  small?: boolean;
}

// eslint-disable-next-line @typescript-eslint/ban-types
export function Table<T extends object>({
  data,
  columns,
  className,
  small,
  mode,
}: LibraryTableProps<T>): React.ReactElement {
  const { getTableProps, getTableBodyProps, headerGroups, rows, prepareRow } = useTable({
    columns,
    data,
  });

  const lastColumnWithRightBorder = computeLastColumnWithRightBorder(columns);

  return (
    <table {...getTableProps()} className={`ednaTable ${className ?? ""}`}>
      <thead>
        {headerGroups.map(headerGroup => (
          <tr {...headerGroup.getHeaderGroupProps()}>
            {headerGroup.headers.map((column, i) => (
              <th {...column.getHeaderProps()} className="ednaTable__columnHead">
                {column.render("Header")}
                {i <= lastColumnWithRightBorder && <span className="ednaTable__headRightBorder" />}
              </th>
            ))}
          </tr>
        ))}
      </thead>
      <tbody {...getTableBodyProps()}>
        {rows.map((row, i) => {
          prepareRow(row);
          return (
            <tr
              {...row.getRowProps()}
              className={cx({ ednaTable__row_withHover: mode === "bordered" })}
            >
              {row.cells.map(cell => {
                return (
                  <td
                    {...cell.getCellProps()}
                    className={`ednaTable__cell
                    ${
                      mode === "alternate"
                        ? i % 2 === 1
                          ? "ednaTable__cell_odd"
                          : i % 2 === 0
                          ? "ednaTable__cell_even"
                          : ""
                        : ""
                    }
                      ${small ? "ednaTable__cell_small" : ""}`}
                  >
                    {cell.render("Cell")}
                  </td>
                );
              })}
            </tr>
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
