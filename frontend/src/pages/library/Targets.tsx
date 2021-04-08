import { useRecoilValue } from "recoil";
import { Column } from "react-table";
import React from "react";
import { Link } from "react-router-dom";
import { targetsQuery } from "../../store/selectors";
import { TargetDto } from "../../api/types";
import { extraFormatter, formatDateTimeDto } from "../../utils/utils";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Button } from "../../components/Button/Button";
import { Table } from "../../components/Table/Table";

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
        accessor: (t: TargetDto) => extraFormatter(t.item.projects),
      },
      {
        Header: "Addition date",
        accessor: (t: TargetDto) => formatDateTimeDto(t.item.additionDate),
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
