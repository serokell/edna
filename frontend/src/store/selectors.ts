// Derived state should be placed here
import { selector } from "recoil";
import {
  compoundIdSelectedAtom,
  compoundsReqIdAtom,
  methodologiesRequestIdAtom,
  projectSelectedIdAtom,
  projectsRequestIdAtom,
  targetIdSelectedAtom,
  targetsRequestIdAtom,
} from "./atoms";
import Api from "../api/api";
import { isDefined, Maybe } from "../utils/utils";
import { CompoundDto, ProjectDto, TargetDto } from "../api/types";

// Library page
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

// Dashboard page
export const projectSelectedQuery = selector<Maybe<ProjectDto>>({
  key: "DashboardProjectSelected",
  get: ({ get }) => {
    const projectId = get(projectSelectedIdAtom);
    if (!isDefined(projectId)) return undefined;
    const projects = get(projectsQuery);
    return projects.find(p => p.id === projectId);
  },
});

export const compoundSelectedQuery = selector<Maybe<CompoundDto>>({
  key: "DashboardCompoundSelected",
  get: ({ get }) => {
    const compoundId = get(compoundIdSelectedAtom);
    if (!isDefined(compoundId)) return undefined;
    const compounds = get(compoundsQuery);
    return compounds.find(p => p.id === compoundId);
  },
});

export const targetSelectedQuery = selector<Maybe<TargetDto>>({
  key: "DashboardTargetSelected",
  get: ({ get }) => {
    const targetId = get(targetIdSelectedAtom);
    if (!isDefined(targetId)) return undefined;
    const targets = get(targetsQuery);
    return targets.find(p => p.id === targetId);
  },
});
