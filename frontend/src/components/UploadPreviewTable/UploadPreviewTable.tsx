// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import "./UploadPreviewTable.scss";
import cx from "classnames";
import { Column } from "react-table";
import { Link } from "react-router-dom";
import { constSelector } from "recoil";
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
  readonly compoundId?: number;
  readonly compoundName: string;
  readonly targetId?: number;
  readonly targetName: string;
  [key: string]: any;
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
        id: "compound",
        accessor: (p: PreviewRow) => `${p.compoundName} ${p.compoundId ? "" : "*"}`,
      },
      {
        Header: "Target",
        id: "target",
        accessor: (p: PreviewRow) => `${p.targetName} ${p.targetId ? "" : "*"}`,
      },

      {
        id: "actions",
        disableSortBy: true,
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

  const data: ReadonlyArray<PreviewRow> = targets.reduce((acc: ReadonlyArray<PreviewRow>, ex) => {
    return ex.compounds.reduce((acc1, compound) => {
      return acc1.concat([
        {
          compoundName: compound.name,
          compoundId: compound.id,
          targetName: ex.target.name,
          targetId: ex.target.id,
        },
      ]);
    }, acc);
  }, []);

  return (
    <div className={cx(["tableContainer", "uploadPreviewTableContainer", className])}>
      <Table
        small
        dataOrQuery={constSelector(data)}
        columns={previewColumns}
        className="uploadPreviewTable"
        columnExtras={{
          actions: { manualCellRendering: true },
        }}
      />
    </div>
  );
}
