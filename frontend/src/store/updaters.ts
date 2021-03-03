// Modifying functions which keeps remote state and atoms in sync
// All updaters return () => (args) => Promise to make possible to call
// an updater in a React component, to receive necessary hooks.

import { RecoilState, useSetRecoilState } from "recoil";
import { methodologiesAtom, projectsAtom } from "./atoms";
import Api from "../api/api";

function appenderUpdater<T, Creator extends (...args: any) => Promise<T>>(
  listState: RecoilState<T[]>,
  apiAction: Creator
) {
  return () => {
    const setter = useSetRecoilState(listState);
    return async (...args: Parameters<Creator>) => {
      const newObj = await apiAction(args);
      setter(objs => objs.concat([newObj]));
      return newObj;
    };
  };
}

export const createProjectUpdater = appenderUpdater(projectsAtom, Api.createProject);

export const createMethodologyUpdater = appenderUpdater(methodologiesAtom, Api.createMethodology);
