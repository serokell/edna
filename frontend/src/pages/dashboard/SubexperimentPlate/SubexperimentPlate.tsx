import React, { useRef, useState } from "react";
import { useRecoilValue, useSetRecoilState } from "recoil";
import cx from "classnames";
import { SubExperimentDto } from "../../../api/types";
import "./SubexperimentPlate.scss";
import {
  modalDialogAtom,
  selectedSubExperimentsColorAtom,
  selectedSubExperimentsIdsAtom,
} from "../../../store/atoms";
import {
  useAddSubExperiment,
  useFilteredExperimentsRefresher,
  useRemoveSubExperiments,
} from "../../../store/updaters";
import { SatisfactoryStatus } from "../../../components/SatisfactoryStatus/SatisfactoryStatus";
import "../IC50Line.scss";
import { formatIC50, useClickOutsideCallback } from "../../../utils/utils";
import cn from "../../../utils/bemUtils";
import { ContextItem } from "../../../components/ContextActions/ContextItems";
import Api from "../../../api/api";

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
  const [contextMenuVisible, setContextMenuVisible] = useState(false);
  const contentActions = cn("contextActions");
  const ref = useRef(null);
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const refreshFiltered = useFilteredExperimentsRefresher();

  useClickOutsideCallback(ref, () => setContextMenuVisible(false));

  // TODO hover for error and value of ic50
  return (
    <div
      onContextMenu={e => {
        setContextMenuVisible(true);
        e.preventDefault();
      }}
      className={cx("subexperimentPlate", className)}
      style={{ borderColor: activeColor }}
      onClick={() => {
        if (selectedSubExperiments.has(subexperiment.id)) {
          removeSubExperiments([subexperiment.id]);
        } else {
          addSubExperiment(subexperiment.id);
        }
      }}
    >
      <div ref={ref} className={contentActions("content", { visible: contextMenuVisible })}>
        {[
          <ContextItem
            type="primary"
            key="primary"
            onClick={async () => {
              await Api.makePrimary(subexperiment.id);
              refreshFiltered();
            }}
          />,
          <ContextItem
            type="rename"
            key="rename"
            onClick={() => {
              setModalDialog({
                kind: "rename-subexperiment",
                name: subexperiment.item.name,
                subId: subexperiment.id,
              });
            }}
          />,
          <ContextItem
            type="delete"
            key="delete"
            onClick={() => {
              setModalDialog({
                kind: "delete-subexperiment",
                subexperiment,
              });
            }}
          />,
        ]}
      </div>
      <div className="subexperimentPlate__head">
        <SatisfactoryStatus
          isSuspicious={subexperiment.item.isSuspicious}
          className="subexperimentPlate__satisfactoryStatus"
        />
        {subexperiment.item.name}
      </div>

      <div className="ic50">
        <span className="ic50__label">IC50</span>

        {"Left" in subexperiment.item.result ? (
          <span className="ic50__valueNone" />
        ) : (
          <span className="ic50__value">{formatIC50(subexperiment.item.result.Right[2])}</span>
        )}
      </div>
    </div>
  );
}
