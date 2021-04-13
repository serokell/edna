import React from "react";
import "./NewSubexperimentPlate.scss";
import { Button } from "../../../components/Button/Button";

interface NewSubexperimentPlateProps {
  ic50: number;
}

export function NewSubexperimentPlate({ ic50 }: NewSubexperimentPlateProps): React.ReactElement {
  return (
    <div className="newSubexperimentPlate">
      <div>New subexperiment</div>
      <div className="ic50">
        <span className="ic50__label">IC50</span>

        <span className="ic50__value">{ic50 || "-"}</span>
      </div>

      <div className="newSubexperimentPlate__btns">
        <Button
          className="newSubexperimentPlate__saveBtn"
          type="half-rounded"
          size="small"
          onClick={() => {}}
        >
          Create new
        </Button>

        <Button type="text" size="small" onClick={() => {}}>
          Cancel
        </Button>
      </div>
    </div>
  );
}
