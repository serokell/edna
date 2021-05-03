// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { useRecoilValue } from "recoil";
import { Column } from "react-table";
import React from "react";
import { Link } from "react-router-dom";
import { defaultBatchQuery, targetsQuery } from "../../store/selectors";
import { TargetDto } from "../../api/types";
import { formatAsDate, formatAsDateTime } from "../../utils/utils";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Button } from "../../components/Button/Button";
import { Table } from "../../components/Table/Table";
import { ExtraFormatter } from "../../components/ExtraFormatter";
import { Tooltip } from "../../components/Tooltip/Tooltip";

export function TargetsSuspendable(): React.ReactElement {
  // TODO request here only 1st page to check projects emptiness
  const targetsChunk = useRecoilValue(defaultBatchQuery(targetsQuery));
  const targetColumns: Column<TargetDto>[] = React.useMemo(
    () => [
      {
        Header: "Target",
        id: "name",
        accessor: (t: TargetDto) => t.item.name,
      },
      {
        Header: "Projects",
        disableSortBy: true,
        accessor: (t: TargetDto) => <ExtraFormatter items={t.item.projects} />,
      },
      {
        Header: "Addition date",
        id: "additionDate",
        accessor: (t: TargetDto) => (
          <Tooltip text={formatAsDateTime(t.item.additionDate)}>
            {formatAsDate(t.item.additionDate)}
          </Tooltip>
        ),
      },
    ],
    []
  );
  return targetsChunk.length === 0 ? (
    <EmptyPlaceholder
      title="No targets added yet"
      description="To add targets add new experiments"
      button={
        <Link to="/upload" className="libraryPage__linkButton">
          <Button type="primary">Add experiments</Button>
        </Link>
      }
    />
  ) : (
    <Table dataOrQuery={targetsQuery} columns={targetColumns} defaultSortedColumn="name" />
  );
}
