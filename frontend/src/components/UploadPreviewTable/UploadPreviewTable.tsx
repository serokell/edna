// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import "./UploadPreviewTable.scss";
import cx from "classnames";
import { Column } from "react-table";
import { Link } from "react-router-dom";
import { Button } from "../Button/Button";
import { ParsedExcelDto } from "../../api/types";
import { Table } from "../Table/Table";
import "../Table/Table.scss";
import { isDefined } from "../../utils/utils";

interface UploadTableProps {
  className?: string;
  targets: ParsedExcelDto[];
  projectId?: number;
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
  projectId,
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
        accessor: (p: PreviewRow) => {
          const disabled = !projectId || !isDefined(p.compoundId) || !isDefined(p.targetId);
          const viewBtn = (
            <Button propagate type="link" disabled={disabled}>
              View
            </Button>
          );
          return (
            <td className="ednaTable__cell uploadPreviewTable__actions">
              {disabled ? (
                viewBtn
              ) : (
                <Link
                  to={{
                    pathname: "/dashboard",
                    state: { projectId, compoundId: p.compoundId, targetId: p.targetId },
                  }}
                >
                  {viewBtn}
                </Link>
              )}
            </td>
          );
        },
      },
    ],
    [projectId]
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
