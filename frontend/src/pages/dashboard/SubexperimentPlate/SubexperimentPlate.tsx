// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { useRef, useState } from "react";
import { useRecoilCallback, useRecoilValue, useSetRecoilState } from "recoil";
import cx from "classnames";
import { SubExperimentDto } from "../../../api/types";
import "./SubexperimentPlate.scss";
import {
  modalDialogAtom,
  selectedSubExperimentsColorAtom,
  selectedSubExperimentsIdsAtom,
  subExperimentsMetaAtom,
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
import { Tooltip } from "../../../components/Tooltip/Tooltip";

interface SubexperimentPlateProps {
  subexperiment: SubExperimentDto;
  isPrimary?: boolean;
  className?: string;
}

export function SubexperimentPlate({
  subexperiment,
  className,
  isPrimary,
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

  const handleSuspiciousChange = useRecoilCallback(({ set }) => async () => {
    const newSub = await Api.changeSuspiciousFlag(
      subexperiment.id,
      !subexperiment.item.isSuspicious
    );
    set(subExperimentsMetaAtom(subexperiment.id), newSub);
  });

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
          onClick={handleSuspiciousChange}
          isSuspicious={subexperiment.item.isSuspicious}
          className="subexperimentPlate__satisfactoryStatus"
        />
        {isPrimary ? <b>{subexperiment.item.name}</b> : subexperiment.item.name}
      </div>

      <div className="ic50">
        <span className="ic50__label">IC50</span>

        {"Left" in subexperiment.item.result ? (
          <Tooltip type="error" text={subexperiment.item.result.Left}>
            <span className="ic50__valueNone" />
          </Tooltip>
        ) : (
          <Tooltip text={`${subexperiment.item.result.Right[2]}`}>
            <span className="ic50__value">{formatIC50(subexperiment.item.result.Right[2])}</span>
          </Tooltip>
        )}
      </div>
    </div>
  );
}
