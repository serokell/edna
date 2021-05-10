// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// Modifying functions which keeps remote state and atoms in sync
// All updaters return () => (args) => Promise to make possible to call
// an updater in a React component, to receive necessary hooks.

import {
  RecoilState,
  useRecoilCallback,
  useRecoilValue,
  useResetRecoilState,
  useSetRecoilState,
} from "recoil";
import {
  colorsCounterAtom,
  compoundIdSelectedAtom,
  compoundsReqIdAtom,
  experimentsTableSizeAtom,
  filteredExperimentsReqIdAtom,
  methodologiesRequestIdAtom,
  newSubexperimentAtom,
  notificationListAtom,
  projectSelectedIdAtom,
  projectsRequestIdAtom,
  selectedSubExperimentsColorAtom,
  selectedSubExperimentsIdsAtom,
  targetIdSelectedAtom,
  targetsRequestIdAtom,
} from "./atoms";
import { minAmountColorQuery } from "./selectors";
import { isDefined } from "../utils/utils";
import { chartColors, NotificationUpdateAction } from "./types";

function useQueryRefresher(reqId: RecoilState<number>): () => void {
  const setReqId = useSetRecoilState(reqId);
  return () => setReqId(req => req + 1);
}

export function useProjectsRefresher(): () => void {
  return useQueryRefresher(projectsRequestIdAtom);
}

export function useCompoundsRefresher(): () => void {
  return useQueryRefresher(compoundsReqIdAtom);
}

export function useTargetsRefresher(): () => void {
  return useQueryRefresher(targetsRequestIdAtom);
}

export function useMethodologiesRefresher(): () => void {
  return useQueryRefresher(methodologiesRequestIdAtom);
}

export function useFilteredExperimentsRefresher(): () => void {
  return useQueryRefresher(filteredExperimentsReqIdAtom);
}

export function useAddSubExperiment(): (subExp: number) => void {
  return useRecoilCallback(
    ({ set, snapshot }) => async (id: number) => {
      const newCol = await snapshot.getPromise(minAmountColorQuery);
      set(colorsCounterAtom(newCol), prevAm => prevAm + 1);
      set(selectedSubExperimentsColorAtom(id), newCol);
      set(selectedSubExperimentsIdsAtom, old => new Set(old.add(id)));
    },
    [selectedSubExperimentsIdsAtom]
  );
}

export function useRemoveSubExperiments(): (subExps: number[]) => void {
  return useRecoilCallback(
    ({ set, snapshot }) => async (ids: number[]) => {
      ids.forEach(async id => {
        const curCol = await snapshot.getPromise(selectedSubExperimentsColorAtom(id));
        if (curCol) set(colorsCounterAtom(curCol), prevAm => prevAm - 1);
        set(selectedSubExperimentsColorAtom(id), undefined);
      });

      const newSub = await snapshot.getPromise(newSubexperimentAtom);

      // If current subexperiment under consideration is removed from set
      if (isDefined(ids.find(sId => sId === newSub.subExperimentId))) {
        set(newSubexperimentAtom, { subExperimentId: -1, changedPoints: [] });
      }

      set(selectedSubExperimentsIdsAtom, old => {
        ids.forEach(id => old.delete(id));
        return new Set(old);
      });
    },
    [selectedSubExperimentsIdsAtom]
  );
}

export function useLibraryRefresher(): () => void {
  const projects = useProjectsRefresher();
  const compounds = useCompoundsRefresher();
  const targets = useTargetsRefresher();
  const methodologies = useMethodologiesRefresher();
  return () => {
    projects();
    compounds();
    targets();
    methodologies();
  };
}

export function useDashboardRefresher(): () => void {
  const resetProjectSelectedId = useResetRecoilState(projectSelectedIdAtom);
  const resetCompoundIdSelected = useResetRecoilState(compoundIdSelectedAtom);
  const resetTargetIdSelected = useResetRecoilState(targetIdSelectedAtom);
  const resetExperimentsTableSizeAtom = useResetRecoilState(experimentsTableSizeAtom);
  const resetNewSubExperiment = useResetRecoilState(newSubexperimentAtom);
  const setSelectedSubExperimentsIds = useSetRecoilState(selectedSubExperimentsIdsAtom);
  const experimentsRefresher = useFilteredExperimentsRefresher();
  const selectedSubExperiments = useRecoilValue(selectedSubExperimentsIdsAtom);

  const resetColorsCounter = useRecoilCallback(({ reset }) => () => {
    for (let i = 0; i < chartColors.length; i++) {
      reset(colorsCounterAtom(chartColors[i]));
    }
  });

  const resetSubexperimentsColors = useRecoilCallback(({ reset }) => () => {
    const arr = Array.from(selectedSubExperiments);
    for (let i = 0; i < arr.length; i++) {
      reset(selectedSubExperimentsColorAtom(arr[i]));
    }
  });

  return () => {
    resetColorsCounter();
    resetSubexperimentsColors();
    resetProjectSelectedId();
    resetCompoundIdSelected();
    resetTargetIdSelected();
    resetNewSubExperiment();
    resetExperimentsTableSizeAtom();
    experimentsRefresher();
    // For some weird reason reset doesn't update selectedExperimentsQuery selector
    setSelectedSubExperimentsIds(new Set<number>());
  };
}

// Notification

export function useNotificationListUpdater(): (act: NotificationUpdateAction) => void {
  return useRecoilCallback(
    ({ set }) => async (action: NotificationUpdateAction) => {
      switch (action.type) {
        case "Delete": {
          set(notificationListAtom, old => {
            const newNotifications = Array.from(old.notifications);
            const realIndex = newNotifications.findIndex(n => n.id === action.id);
            newNotifications.splice(realIndex, 1);
            return { lastId: old.lastId, notifications: newNotifications };
          });
          break;
        }
        case "Add": {
          set(notificationListAtom, old => ({
            lastId: old.lastId + 1,
            notifications: [
              ...old.notifications,
              { id: old.lastId + 1, element: action.element, type: action.notificationType },
            ],
          }));
          break;
        }
        default:
          break;
      }
    },
    [notificationListAtom]
  );
}
