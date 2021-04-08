import React from "react";
import { useRecoilValue } from "recoil";
import cx from "classnames";
import { SubExperimentDto } from "../../api/types";
import StatusSvg from "../../assets/svg/status.svg";
import "./SubexperimentPlate.scss";
import { selectedSubExperimentsColorAtom, selectedSubExperimentsIdsAtom } from "../../store/atoms";
import { useAddSubExperiment, useRemoveSubExperiments } from "../../store/updaters";

interface SubexperimentPlateProps {
  subexperiment: SubExperimentDto;
}

export function SubexperimentPlate({ subexperiment }: SubexperimentPlateProps): React.ReactElement {
  const activeColor = useRecoilValue(selectedSubExperimentsColorAtom(subexperiment.id));
  const selectedSubExperiments = useRecoilValue(selectedSubExperimentsIdsAtom);
  const addSubExperiment = useAddSubExperiment();
  const removeSubExperiments = useRemoveSubExperiments();

  return (
    <div
      className="subexperimentPlate"
      style={{ borderColor: activeColor }}
      onClick={() => {
        if (selectedSubExperiments.has(subexperiment.id)) removeSubExperiments([subexperiment.id]);
        else addSubExperiment(subexperiment.id);
      }}
    >
      <div className="subexperimentPlate__head">
        <StatusSvg
          className={cx("satisfactoryStatus", "subexperimentPlate__satisfactoryStatus", {
            satisfactoryStatus__ok: !subexperiment.item.isSuspicious,
            satisfactoryStatus__bad: subexperiment.item.isSuspicious,
          })}
        />
        {subexperiment.item.name}
      </div>

      <div className="subexperimentPlate__ic50">
        <span className="subexperimentPlate__ic50label">IC50</span>

        <span className="subexperimentPlate__ic50value">{subexperiment.item.result[2]}</span>
      </div>
    </div>
  );
}
