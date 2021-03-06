// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { useRecoilValue, useSetRecoilState } from "recoil";
import React from "react";
import { modalDialogAtom } from "../../store/atoms";
import { defaultBatchQuery, projectsQuery } from "../../store/selectors";
import { ProjectDto } from "../../api/types";
import { formatAsDate, formatAsDateTime } from "../../utils/utils";
import { ContextActions } from "../../components/ContextActions/ContextActions";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Button } from "../../components/Button/Button";
import { Table } from "../../components/Table/Table";
import { ContextItem } from "../../components/ContextActions/ContextItems";
import { ExtraFormatter } from "../../components/ExtraFormatter";
import { Tooltip } from "../../components/Tooltip/Tooltip";

export function ProjectsSuspendable(): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  // TODO request here only 1st page to check projects emptiness
  const projectsChunk = useRecoilValue(defaultBatchQuery(projectsQuery));

  const projectColumns = React.useMemo(
    () => [
      {
        Header: "Project",
        id: "name",
        accessor: (p: ProjectDto) => p.item.name,
      },
      {
        Header: "Compounds",
        disableSortBy: true,
        accessor: (p: ProjectDto) => (
          <span className="project__compounds">
            <ExtraFormatter items={p.item.compoundNames} />
          </span>
        ),
      },
      {
        Header: "Creation date",
        id: "creationDate",
        accessor: (p: ProjectDto) => (
          <Tooltip text={formatAsDateTime(p.item.creationDate)}>
            {formatAsDate(p.item.creationDate)}
          </Tooltip>
        ),
      },
      {
        Header: "Last update",
        id: "lastUpdate",
        accessor: (p: ProjectDto) => (
          <Tooltip text={formatAsDateTime(p.item.lastUpdate)}>
            {formatAsDate(p.item.lastUpdate)}
          </Tooltip>
        ),
      },
      {
        id: "actions",
        disableSortBy: true,
        accessor: (p: ProjectDto) => (
          <ContextActions
            actions={[
              <ContextItem
                key="edit"
                type="edit"
                onClick={() =>
                  setModalDialog({
                    kind: "create-edit-project",
                    editing: p,
                  })
                }
              />,
            ]}
          />
        ),
      },
    ],
    [setModalDialog]
  );
  return projectsChunk.length === 0 ? (
    <EmptyPlaceholder
      title="No projects created yet"
      description="All created projects will be displayed here"
      button={
        <Button type="primary" onClick={() => setModalDialog({ kind: "create-edit-project" })}>
          Create project
        </Button>
      }
    />
  ) : (
    <Table dataOrQuery={projectsQuery} columns={projectColumns} defaultSortedColumn="name" />
  );
}
