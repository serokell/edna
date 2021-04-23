// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { useRecoilValue } from "recoil";
import { Column } from "react-table";
import React from "react";
import { Link } from "react-router-dom";
import { targetsQuery } from "../../store/selectors";
import { TargetDto } from "../../api/types";
import { formatAsDate, formatAsDateTime } from "../../utils/utils";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Button } from "../../components/Button/Button";
import { Table } from "../../components/Table/Table";
import { ExtraFormatter } from "../../components/ExtraFormatter";
import { Tooltip } from "../../components/Tooltip/Tooltip";

export function TargetsSuspendable(): React.ReactElement {
  const targets = useRecoilValue(targetsQuery);
  const targetColumns: Column<TargetDto>[] = React.useMemo(
    () => [
      {
        Header: "Target",
        accessor: (t: TargetDto) => t.item.name,
      },
      {
        Header: "Projects",
        accessor: (t: TargetDto) => <ExtraFormatter items={t.item.projects} />,
      },
      {
        Header: "Addition date",
        accessor: (t: TargetDto) => (
          <Tooltip text={formatAsDateTime(t.item.additionDate)}>
            {formatAsDate(t.item.additionDate)}
          </Tooltip>
        ),
      },
    ],
    []
  );
  return targets.length === 0 ? (
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
    <Table data={targets} columns={targetColumns} />
  );
}
