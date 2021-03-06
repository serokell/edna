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
  subExperimentsMetaMap,
} from "../../../store/atoms";
import {
  useAddSubExperiment,
  useFilteredExperimentsRefresher,
  useNotificationListUpdater,
  useRemoveSubExperiments,
} from "../../../store/updaters";
import { SatisfactoryStatus } from "../../../components/SatisfactoryStatus/SatisfactoryStatus";
import { useClickOutsideCallback } from "../../../utils/utils";
import cn from "../../../utils/bemUtils";
import { ContextItem } from "../../../components/ContextActions/ContextItems";
import Api from "../../../api/api";
import { IC50Line } from "../../../components/IC50Line/IC50Line";

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
  const notificationsUpdater = useNotificationListUpdater();

  useClickOutsideCallback(ref, () => setContextMenuVisible(false));

  const handleSuspiciousChange = useRecoilCallback(({ set }) => async () => {
    const newSub = await Api.changeSuspiciousFlag(
      subexperiment.id,
      !subexperiment.item.isSuspicious
    );
    set(subExperimentsMetaMap(subexperiment.id), newSub);
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
        if ("Left" in subexperiment.item.result) {
          notificationsUpdater({
            type: "Add",
            notificationType: "Warn",
            element: () => (
              // eslint-disable-next-line @typescript-eslint/ban-ts-comment
              // @ts-ignore
              <span>{`This sub-experiment is invalid because of aproximation error: ${subexperiment.item.result.Left}`}</span>
            ),
          });
          return;
        }
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
              if (isPrimary) {
                notificationsUpdater({
                  type: "Add",
                  notificationType: "Warn",
                  element: () => <span>You are not alowed to delete primary sub-experiment</span>,
                });
              } else {
                setModalDialog({
                  kind: "delete-subexperiment",
                  subexperiment,
                });
              }
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
      <IC50Line label="IC50" ic50={subexperiment.item.result} />
    </div>
  );
}
