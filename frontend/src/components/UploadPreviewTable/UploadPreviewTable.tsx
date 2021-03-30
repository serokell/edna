import React from "react";
import "./UploadPreviewTable.scss";
import cx from "classnames";
import { Button } from "../Button/Button";
import { ParsedExcelDto } from "../../api/types";
import { Table } from "../Table/Table";

interface UploadTableProps {
  className?: string;
  targets: ParsedExcelDto[];
  viewEnabled: boolean;
}

interface PreviewRow {
  compound: string;
  target: string;
  actions: React.ReactNode;
}

export function UploadPreviewTable({
  className,
  targets,
  viewEnabled,
}: UploadTableProps): React.ReactElement {
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
        compound: `${compound.name} ${compound.id ? "" : "*"}`,
        target: `${ex.target.name} ${ex.target.id ? "" : "*"}`,
        actions: (
          <div className="uploadPreviewTable__actions">
            <Button type="link" disabled={!viewEnabled}>
              View
            </Button>
          </div>
        ),
      });
    })
  );

  return (
    <div className={cx(["uploadPreviewTableContainer", className])}>
      <Table small data={data} columns={previewColumns} className="uploadPreviewTable" />
    </div>
  );
}
