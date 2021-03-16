import React from "react";
import { v4 as uuidv4 } from "uuid";
import "./UploadPreviewTable.scss";
import cx from "classnames";
import { Button } from "../Button/Button";
import { ParsedTargetDto } from "../../api/types";

interface UploadTableProps {
  className?: string;
  targets: ParsedTargetDto[];
}

// TODO this table will be styled properly in library task
export function UploadPreviewTable({ className, targets }: UploadTableProps): React.ReactElement {
  return (
    <table className={cx(["uploadPreviewTable", className])}>
      <thead>
        <tr>
          <th>Compound</th>
          <th>Target</th>
          <th />
        </tr>
      </thead>

      <tbody>
        {targets.map(ex =>
          ex.compounds.map(cp => (
            <tr key={uuidv4()}>
              <td>{cp}</td>
              <td>
                {ex.target}
                {ex.isNew ? "*" : ""}
              </td>
              <td className="uploadPreviewTable__actions">
                <Button type="link" disabled>
                  View
                </Button>
              </td>
            </tr>
          ))
        )}
      </tbody>
    </table>
  );
}
