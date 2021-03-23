import React from "react";
import "./UploadPreviewTable.scss";
import cx from "classnames";
import { Button } from "../Button/Button";
import { ParsedTargetDto } from "../../api/types";
import { Table } from "../Table/Table";

interface UploadTableProps {
  className?: string;
  targets: ParsedTargetDto[];
}

interface PreviewRow {
  compound: string;
  target: string;
  actions: React.ReactNode;
}

export function UploadPreviewTable({ className, targets }: UploadTableProps): React.ReactElement {
  const previewColumns = React.useMemo(
    () => [
      {
        Header: "Compound",
        accessor: "compound" as const, // accessor is the "key" in the data
      },
      {
        Header: "Target",
        accessor: "target" as const,
      },

      {
        accessor: "actions" as const,
      },
    ],
    []
  );

  const data: PreviewRow[] = [];
  targets.forEach(ex =>
    ex.compounds.forEach(compound => {
      data.push({
        compound,
        target: `${ex.target} ${ex.isNew ? "*" : ""}`,
        actions: (
          <div className="uploadPreviewTable__actions">
            <Button type="link" disabled>
              View
            </Button>
          </div>
        ),
      });
    })
  );

  return (
    <div className="uploadPreviewTableContainer">
      <Table
        mode="alternate"
        small
        data={data}
        columns={previewColumns}
        className={cx(["uploadPreviewTable", className])}
      />
    </div>
  );
}
