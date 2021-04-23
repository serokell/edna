// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import "./UploadPreviewTable.scss";
import cx from "classnames";
import { Column } from "react-table";
import { Button } from "../Button/Button";
import { ParsedExcelDto } from "../../api/types";
import { Table } from "../Table/Table";
import "../Table/Table.scss";

interface UploadTableProps {
  className?: string;
  targets: ParsedExcelDto[];
  viewEnabled: boolean;
}

interface PreviewRow {
  compoundId?: number;
  compoundName: string;
  targetId?: number;
  targetName: string;
}

export function UploadPreviewTable({
  className,
  targets,
  viewEnabled,
}: UploadTableProps): React.ReactElement {
  const previewColumns: Column<PreviewRow>[] = React.useMemo(
    () => [
      {
        Header: "Compound",
        accessor: (p: PreviewRow) => `${p.compoundName} ${p.compoundId ? "" : "*"}`,
      },
      {
        Header: "Target",
        accessor: (p: PreviewRow) => `${p.targetName} ${p.targetId ? "" : "*"}`,
      },

      {
        id: "actions",
        accessor: () => (
          // TODO add link
          <td className="ednaTable__cell uploadPreviewTable__actions">
            <Button type="link" disabled={!viewEnabled}>
              View
            </Button>
          </td>
        ),
      },
    ],
    [viewEnabled]
  );

  const data: PreviewRow[] = [];
  targets.forEach(ex =>
    ex.compounds.forEach(compound => {
      data.push({
        compoundName: compound.name,
        compoundId: compound.id,
        targetName: ex.target.name,
        targetId: ex.target.id,
      });
    })
  );

  return (
    <div className={cx(["tableContainer", "uploadPreviewTableContainer", className])}>
      <Table
        small
        data={data}
        columns={previewColumns}
        className="uploadPreviewTable"
        columnExtras={{
          actions: { manualCellRendering: true },
        }}
      />
    </div>
  );
}
