// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { useRecoilValue, useSetRecoilState } from "recoil";
import React from "react";
import { Link } from "react-router-dom";
import { compoundsQuery, defaultBatchQuery } from "../../store/selectors";
import { modalDialogAtom } from "../../store/atoms";
import { CompoundDto } from "../../api/types";
import { Button } from "../../components/Button/Button";
import { ContextActions } from "../../components/ContextActions/ContextActions";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Table } from "../../components/Table/Table";
import { ContextItem } from "../../components/ContextActions/ContextItems";

export function CompoundsSuspendable(): React.ReactElement {
  // TODO request here only 1st page to check compounds emptiness
  const compoundsChunk = useRecoilValue(defaultBatchQuery(compoundsQuery));
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const compoundsColumns = React.useMemo(
    () => [
      {
        Header: "Compound",
        id: "name",
        accessor: (c: CompoundDto) => c.item.name,
      },
      {
        Header: "MDe link",
        id: "mde",
        disableSortBy: true,
        accessor: (c: CompoundDto) => (
          <td className={`ednaTable__cell${c.item.mde ? "" : " libraryTable__cellBtn"}`}>
            {c.item.mde ? (
              <a href={c.item.mde}>{c.item.mde}</a>
            ) : (
              <Button
                type="half-rounded"
                size="small"
                onClick={() => {
                  setModalDialog({
                    kind: "add-edit-link",
                    target: { kind: "compound-mde", object: c },
                  });
                }}
              >
                Add link
              </Button>
            )}
          </td>
        ),
      },
      {
        Header: "ChemSoft link",
        disableSortBy: true,
        id: "chemsoft",
        accessor: (c: CompoundDto) => (
          <td className={`ednaTable__cell${c.item.chemSoft ? "" : " libraryTable__cellBtn"}`}>
            {c.item.chemSoft ? (
              <a href={c.item.chemSoft}>{c.item.chemSoft}</a>
            ) : (
              <Button
                type="half-rounded"
                size="small"
                onClick={() => {
                  setModalDialog({
                    kind: "add-edit-link",
                    target: { kind: "compound-chemsoft", object: c },
                  });
                }}
              >
                Add link
              </Button>
            )}
          </td>
        ),
      },
      {
        id: "actions",
        disableSortBy: true,
        accessor: (c: CompoundDto) => (
          <ContextActions
            actions={[
              <ContextItem
                key="edit-chemsoft"
                type="edit"
                value="Edit ChemSoft"
                onClick={() => {
                  setModalDialog({
                    kind: "add-edit-link",
                    target: { kind: "compound-chemsoft", object: c },
                  });
                }}
              />,
              <ContextItem
                key="edit-mde"
                type="edit"
                value="Edit MDe"
                onClick={() => {
                  setModalDialog({
                    kind: "add-edit-link",
                    target: { kind: "compound-mde", object: c },
                  });
                }}
              />,
            ]}
          />
        ),
      },
    ],
    [setModalDialog]
  );
  return compoundsChunk.length === 0 ? (
    <EmptyPlaceholder
      title="No compounds added yet"
      description="To add compounds add new experiments"
      button={
        <Link to="/upload" className="libraryPage__linkButton">
          <Button type="primary">Add experiments</Button>
        </Link>
      }
    />
  ) : (
    <Table
      dataOrQuery={compoundsQuery}
      defaultSortedColumn="name"
      columns={compoundsColumns}
      columnExtras={{
        chemsoft: { manualCellRendering: true },
        mde: { manualCellRendering: true },
      }}
    />
  );
}
