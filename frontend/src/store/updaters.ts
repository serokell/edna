// Modifying functions which keeps remote state and atoms in sync
// All updaters return () => (args) => Promise to make possible to call
// an updater in a React component, to receive necessary hooks.

import { RecoilState, useRecoilCallback, useSetRecoilState } from "recoil";
import {
  compoundsReqIdAtom,
  methodologiesRequestIdAtom,
  projectsRequestIdAtom,
  selectedSubExperimentsIdsAtom,
  targetsRequestIdAtom,
} from "./atoms";

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

export function useAddSubExperiment(): (subExp: number) => void {
  return useRecoilCallback(
    ({ set }) => (id: number) => {
      set(selectedSubExperimentsIdsAtom, old => new Set(old.add(id)));
    },
    [selectedSubExperimentsIdsAtom]
  );
}

export function useRemoveSubExperiments(): (subExps: number[]) => void {
  return useRecoilCallback(
    ({ set }) => (ids: number[]) => {
      set(selectedSubExperimentsIdsAtom, old => {
        ids.forEach(id => old.delete(id));
        return new Set(old);
      });
    },
    [selectedSubExperimentsIdsAtom]
  );
}
