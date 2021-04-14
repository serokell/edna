// Modifying functions which keeps remote state and atoms in sync
// All updaters return () => (args) => Promise to make possible to call
// an updater in a React component, to receive necessary hooks.

import { RecoilState, useRecoilCallback, useSetRecoilState } from "recoil";
import {
  colorsCounterAtom,
  compoundsReqIdAtom,
  filteredExperimentsReqIdAtom,
  methodologiesRequestIdAtom,
  newSubexperimentAtom,
  projectsRequestIdAtom,
  selectedSubExperimentsColorAtom,
  selectedSubExperimentsIdsAtom,
  targetsRequestIdAtom,
} from "./atoms";
import { minAmountColor } from "./selectors";
import { isDefined } from "../utils/utils";

function useQueryRefresher(reqId: RecoilState<number>): () => void {
  const setReqId = useSetRecoilState(reqId);
  return () => setReqId(req => req + 1);
}

export function useProjectRefresher(): () => void {
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
      const newCol = await snapshot.getPromise(minAmountColor);
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
