import React from "react";
import { Column, useTable } from "react-table";
import "./Table.scss";
import cx from "classnames";

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
}

// eslint-disable-next-line @typescript-eslint/ban-types
export function Table<T extends object>({
  data,
  columns,
  columnExtras,
  className,
  small,
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
            <tr
              {...row.getRowProps()}
              className={`ednaTable__row ${
                i % 2 === 0 ? "ednaTable__row_odd" : "ednaTable__row_even"
              }`}
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
                  <td
                    {...cell.getCellProps()}
                    className={`ednaTable__cell ${cx({ ednaTable__cell_small: small })}`}
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
