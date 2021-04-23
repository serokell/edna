// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { useState } from "react";
import "./NewSubexperimentPlate.scss";
import cx from "classnames";
import { useRecoilState, useSetRecoilState } from "recoil";
import { Button } from "../../../components/Button/Button";
import { modalDialogAtom, newSubexperimentAtom } from "../../../store/atoms";
import { formatIC50, isDefined } from "../../../utils/utils";
import RecomputeSvg from "../../../assets/svg/recompute.svg";
import Api from "../../../api/api";
import { useFilteredExperimentsRefresher } from "../../../store/updaters";
import { Tooltip } from "../../../components/Tooltip/Tooltip";

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
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const [expanded, setExpanded] = useState(true);

  return (
    <div
      className={cx("newSubexperimentPlate", className)}
      onClick={() => {
        setExpanded(!expanded);
      }}
    >
      {expanded ? (
        <input
          className="ednaInput newSubexperimentPlate__name"
          value={suexperimentName}
          onChange={e => setSuexperimentName(e.target.value)}
          onClick={e => e.stopPropagation()}
        />
      ) : (
        suexperimentName
      )}
      {expanded && (
        <>
          <div className="ic50 newSubexperimentPlate__ic50">
            <span className="ic50__label">IC50</span>
            {isDefined(ic50) ? (
              <span className="ic50__value">{formatIC50(ic50)}</span>
            ) : (
              <>
                <span className="ic50__valueNone" />
                <Tooltip text="Recompute">
                  <RecomputeSvg
                    className="newSubexperimentPlate__recomouteBtn"
                    onClick={async (e: any) => {
                      e.stopPropagation();
                      const newResult = await Api.analyzeSubexperiment(
                        newSubexperiment.subExperimentId,
                        {
                          name: suexperimentName,
                          changes: newSubexperiment.changedPoints.map(x => x.id),
                        }
                      );
                      if ("Left" in newResult) {
                        setModalDialog({ kind: "failed-recompute-ic50", reason: newResult.Left });
                      } else {
                        setNewSubexperiment(old => ({
                          ...old,
                          analysed: newResult.Right,
                        }));
                      }
                    }}
                  />
                </Tooltip>
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
        </>
      )}
    </div>
  );
}
