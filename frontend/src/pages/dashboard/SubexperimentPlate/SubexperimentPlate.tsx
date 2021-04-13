import React from "react";
import { useRecoilValue } from "recoil";
import cx from "classnames";
import { SubExperimentDto } from "../../../api/types";
import "./SubexperimentPlate.scss";
import {
  selectedSubExperimentsColorAtom,
  selectedSubExperimentsIdsAtom,
} from "../../../store/atoms";
import { useAddSubExperiment, useRemoveSubExperiments } from "../../../store/updaters";
import { SatisfactoryStatus } from "../../../components/SatisfactoryStatus/SatisfactoryStatus";
import "../IC50Line.scss";

interface SubexperimentPlateProps {
  subexperiment: SubExperimentDto;
  className?: string;
}

export function SubexperimentPlate({
  subexperiment,
  className,
}: SubexperimentPlateProps): React.ReactElement {
  const activeColor = useRecoilValue(selectedSubExperimentsColorAtom(subexperiment.id));
  const selectedSubExperiments = useRecoilValue(selectedSubExperimentsIdsAtom);
  const addSubExperiment = useAddSubExperiment();
  const removeSubExperiments = useRemoveSubExperiments();

  return (
    <div
      className={cx("subexperimentPlate", className)}
      style={{ borderColor: activeColor }}
      onClick={() => {
        if (selectedSubExperiments.has(subexperiment.id)) removeSubExperiments([subexperiment.id]);
        else addSubExperiment(subexperiment.id);
      }}
    >
      <div className="subexperimentPlate__head">
        <SatisfactoryStatus
          isSuspicious={subexperiment.item.isSuspicious}
          className="subexperimentPlate__satisfactoryStatus"
        />
        {subexperiment.item.name}
      </div>

      <div className="ic50">
        <span className="ic50__label">IC50</span>

        <span className="ic50__value">{subexperiment.item.result[2]}</span>
      </div>
    </div>
  );
}
