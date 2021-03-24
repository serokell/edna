// Modifying functions which keeps remote state and atoms in sync
// All updaters return () => (args) => Promise to make possible to call
// an updater in a React component, to receive necessary hooks.

import { RecoilState, useSetRecoilState } from "recoil";
import {
  compoundsReqIdAtom,
  methodologiesRequestIdAtom,
  projectsRequestIdAtom,
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
