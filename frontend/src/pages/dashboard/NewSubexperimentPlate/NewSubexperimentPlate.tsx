import React, { useState } from "react";
import "./NewSubexperimentPlate.scss";
import cx from "classnames";
import { useRecoilState } from "recoil";
import { Button } from "../../../components/Button/Button";
import { newSubexperimentAtom } from "../../../store/atoms";
import { isDefined } from "../../../utils/utils";
import RecomputeSvg from "../../../assets/svg/recompute.svg";
import Api from "../../../api/api";
import { useFilteredExperimentsRefresher } from "../../../store/updaters";

interface NewSubexperimentPlateProps {
  className?: string;
}

export function NewSubexperimentPlate({
  className,
}: NewSubexperimentPlateProps): React.ReactElement {
  const [newSubexperiment, setNewSubexperiment] = useRecoilState(newSubexperimentAtom);
  const filteredExperimentsRefresher = useFilteredExperimentsRefresher();
  const ic50 = newSubexperiment.analysed ? newSubexperiment.analysed[2] : undefined;
  const [suexperimentName, setSuexperimentName] = useState<string>("New subexperiment");

  return (
    <div className={cx("newSubexperimentPlate", className)}>
      <input
        className="ednaInput newSubexperimentPlate__name"
        value={suexperimentName}
        onChange={e => setSuexperimentName(e.target.value)}
      />
      <div className="ic50 newSubexperimentPlate__ic50">
        <span className="ic50__label">IC50</span>
        {isDefined(ic50) ? (
          <span className="ic50__value">{ic50}</span>
        ) : (
          <>
            <span className="ic50__valueNone" />
            <RecomputeSvg
              className="newSubexperimentPlate__recomouteBtn"
              onClick={async () => {
                const newResult = await Api.analyzeSubexperiment(newSubexperiment.subExperimentId, {
                  name: suexperimentName,
                  changes: newSubexperiment.changedPoints.map(x => x.id),
                });
                setNewSubexperiment(old => ({
                  ...old,
                  analysed: newResult,
                }));
              }}
            />
          </>
        )}
      </div>

      <div className="newSubexperimentPlate__btns">
        <Button
          className="newSubexperimentPlate__saveBtn"
          type="half-rounded"
          size="small"
          onClick={async () => {
            await Api.newSubexperiment(newSubexperiment.subExperimentId, {
              name: suexperimentName,
              changes: newSubexperiment.changedPoints.map(x => x.id),
            });
            filteredExperimentsRefresher();
            setNewSubexperiment({
              subExperimentId: -1,
              changedPoints: [],
            });
          }}
        >
          Create
        </Button>

        <Button
          className="newSubexperimentPlate__cancelBtn"
          type="text"
          size="small"
          onClick={() => {
            setNewSubexperiment({ subExperimentId: -1, changedPoints: [] });
          }}
        >
          Cancel
        </Button>
      </div>
    </div>
  );
}
