// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { useRecoilValue, useSetRecoilState } from "recoil";
import { Column } from "react-table";
import React from "react";
import { modalDialogAtom } from "../../store/atoms";
import { methodologiesQuery } from "../../store/selectors";
import { MethodologyDto } from "../../api/types";
import { Button } from "../../components/Button/Button";
import { ContextActions } from "../../components/ContextActions/ContextActions";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Table } from "../../components/Table/Table";
import { ContextItem } from "../../components/ContextActions/ContextItems";
import { ExtraFormatter } from "../../components/ExtraFormatter";

export function MethodsSuspendable(): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  // TODO request here only 1st page to check methodologies emptiness
  const methodologiesChunk = useRecoilValue(methodologiesQuery({}));
  const methodologyColumns: Column<MethodologyDto>[] = React.useMemo(
    () => [
      {
        Header: "Methodology",
        accessor: (t: MethodologyDto) => t.item.name,
      },
      {
        Header: "Projects",
        disableSortBy: true,
        accessor: (t: MethodologyDto) => <ExtraFormatter items={t.item.projects} />,
      },
      {
        Header: "Confluence link",
        disableSortBy: true,
        id: "link",
        accessor: (m: MethodologyDto) => {
          if (m.item.confluence)
            return (
              <td className="ednaTable__cell">
                <a href={m.item.confluence}>{m.item.confluence}</a>
              </td>
            );
          return (
            <td className="ednaTable__cell libraryTable__cellBtn">
              <Button
                type="half-rounded"
                size="small"
                onClick={() => {
                  setModalDialog({
                    kind: "add-edit-link",
                    target: { kind: "methodology", object: m },
                  });
                }}
              >
                Add link
              </Button>
            </td>
          );
        },
      },
      {
        Header: "Description",
        disableSortBy: true,
        id: "description",
        accessor: (m: MethodologyDto) => {
          return (
            <td className="ednaTable__cell libraryTable__cellBtn">
              <Button
                type="half-rounded"
                size="small"
                onClick={() => {
                  setModalDialog({
                    kind: "methodology-description",
                    methodology: m,
                  });
                }}
              >
                Show description
              </Button>
            </td>
          );
        },
      },

      {
        id: "actions",
        disableSortBy: true,
        accessor: (m: MethodologyDto) => (
          <ContextActions
            actions={[
              <ContextItem
                type="edit"
                key="edit"
                onClick={() => {
                  setModalDialog({
                    kind: "create-edit-methodology",
                    editing: m,
                  });
                }}
              />,
              <ContextItem
                type="delete"
                key="delete"
                onClick={() => {
                  setModalDialog({
                    kind: "delete-methodology",
                    methodology: m,
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

  return methodologiesChunk.length === 0 ? (
    <EmptyPlaceholder
      title="No methodologies created yet"
      description="All created methodologies will be displayed here"
      button={
        <Button type="primary" onClick={() => setModalDialog({ kind: "create-edit-methodology" })}>
          Create methodology
        </Button>
      }
    />
  ) : (
    <Table
      dataOrQuery={methodologiesQuery}
      columns={methodologyColumns}
      columnExtras={{
        link: { manualCellRendering: true },
        description: { manualCellRendering: true },
      }}
    />
  );
}
