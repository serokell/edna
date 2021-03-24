// Derived state should be placed here
import { selector } from "recoil";
import {
  compoundsReqIdAtom,
  methodologiesRequestIdAtom,
  projectsRequestIdAtom,
  targetsRequestIdAtom,
} from "./atoms";
import Api from "../api/api";

export const projectsQuery = selector({
  key: "projectsQuery",
  get: async ({ get }) => {
    get(projectsRequestIdAtom); // Add request ID as a dependency
    return Api.fetchProjects();
  },
});

export const methodologiesQuery = selector({
  key: "methodologiesQuery",
  get: async ({ get }) => {
    get(methodologiesRequestIdAtom); // Add request ID as a dependency
    return Api.fetchMethodologies();
  },
});

export const compoundsQuery = selector({
  key: "compoundsQuery",
  get: async ({ get }) => {
    get(compoundsReqIdAtom); // Add request ID as a dependency
    return Api.fetchCompounds();
  },
});

export const targetsQuery = selector({
  key: "targetsQuery",
  get: async ({ get }) => {
    get(targetsRequestIdAtom); // Add request ID as a dependency
    return Api.fetchTargets();
  },
});
