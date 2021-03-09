import React from "react";
import { Column, useTable } from "react-table";
import "./LibraryTable.scss";

// eslint-disable-next-line @typescript-eslint/ban-types
interface LibraryTableProps<T extends object> {
  data: T[];
  columns: Column<T>[];
}

// eslint-disable-next-line @typescript-eslint/ban-types
export function LibraryTable<T extends object>({
  data,
  columns,
}: LibraryTableProps<T>): React.ReactElement {
  const { getTableProps, getTableBodyProps, headerGroups, rows, prepareRow } = useTable({
    columns,
    data,
  });

  return (
    <table {...getTableProps()} className="libraryTable">
      <thead>
        {headerGroups.map(headerGroup => (
          <tr {...headerGroup.getHeaderGroupProps()} className="libraryTable__head">
            {headerGroup.headers.map(column => (
              <th {...column.getHeaderProps()} className="libraryTable__columnHead">
                {column.render("Header")}
                <span className="libraryTable__headRightBorder" />
              </th>
            ))}
          </tr>
        ))}
      </thead>
      <tbody {...getTableBodyProps()}>
        {rows.map(row => {
          prepareRow(row);
          return (
            <tr {...row.getRowProps()} className="libraryTable__row">
              {row.cells.map(cell => {
                return (
                  <td {...cell.getCellProps()} className="libraryTable__cell">
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
