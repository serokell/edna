// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// Derived state should be placed here
import { RecoilValueReadOnly, selector, selectorFamily, waitForAll } from "recoil";
import {
  colorsCounterAtom,
  compoundIdSelectedAtom,
  compoundsReqIdAtom,
  filteredExperimentsReqIdAtom,
  methodologiesRequestIdAtom,
  projectSelectedIdAtom,
  projectsRequestIdAtom,
  selectedSubExperimentsIdsAtom,
  subExperimentsMeasurements,
  subExperimentsMetaAtom,
  targetIdSelectedAtom,
  targetsRequestIdAtom,
} from "./atoms";
import Api from "../api/api";
import { isDefined, Maybe } from "../utils/utils";
import {
  CompoundDto,
  ExperimentsWithMeanDto,
  ProjectDto,
  ResultDto,
  TargetDto,
} from "../api/types";
import {
  chartColors,
  Experiment,
  ExperimentsWithMean,
  SubExperimentWithMeasurements,
} from "./types";
import { SortParamsApi } from "../api/EdnaApi";

// Library page
export const projectsQuery = selectorFamily({
  key: "projectsQuery",
  get: (sortingParams: SortParamsApi) => async ({ get }) => {
    // Add request ID as a dependency, to enforce refresh of all sorting params
    get(projectsRequestIdAtom);
    return Api.fetchProjects(sortingParams);
  },
});

export const methodologiesQuery = selectorFamily({
  key: "methodologiesQuery",
  get: (sortingParams: SortParamsApi) => async ({ get }) => {
    // Add request ID as a dependency, to enforce refresh of all sorting params
    get(methodologiesRequestIdAtom);
    return Api.fetchMethodologies(sortingParams);
  },
});

export const compoundsQuery = selectorFamily({
  key: "compoundsQuery",
  get: (sortingParams: SortParamsApi) => async ({ get }) => {
    get(compoundsReqIdAtom); // Add request ID as a dependency
    return Api.fetchCompounds(sortingParams);
  },
});

export const targetsQuery = selectorFamily({
  key: "targetsQuery",
  get: (sortingParams: SortParamsApi) => async ({ get }) => {
    get(targetsRequestIdAtom); // Add request ID as a dependency
    return Api.fetchTargets(sortingParams);
  },
});

export function defaultBatchQuery<T>(
  sel: (params: SortParamsApi) => RecoilValueReadOnly<T>,
  params?: SortParamsApi
): RecoilValueReadOnly<T> {
  if (params) {
    return sel(params);
  }
  return sel({ sortby: "name", desc: false });
}

// Dashboard page
export const projectSelectedQuery = selector<Maybe<ProjectDto>>({
  key: "DashboardProjectSelected",
  get: ({ get }) => {
    const projectId = get(projectSelectedIdAtom);
    if (!isDefined(projectId)) return undefined;
    const projects = get(projectsQuery({}));
    return projects.find(p => p.id === projectId);
  },
});

export const compoundSelectedQuery = selector<Maybe<CompoundDto>>({
  key: "DashboardCompoundSelected",
  get: ({ get }) => {
    const compoundId = get(compoundIdSelectedAtom);
    if (!isDefined(compoundId)) {
      return undefined;
    }
    const compounds = get(compoundsQuery({}));
    return compounds.find(p => p.id === compoundId);
  },
});

export const targetSelectedQuery = selector<Maybe<TargetDto>>({
  key: "DashboardTargetSelected",
  get: ({ get }) => {
    const targetId = get(targetIdSelectedAtom);
    if (!isDefined(targetId)) {
      return undefined;
    }
    const targets = get(targetsQuery({}));
    return targets.find(p => p.id === targetId);
  },
});

export const subExperimentsWithMeasurementsQuery = selectorFamily<
  SubExperimentWithMeasurements,
  number
>({
  key: "SubExperiments",
  get: subExperimentId => async ({ get }) => {
    const [meta, measurements] = get(
      waitForAll([
        subExperimentsMetaAtom(subExperimentId),
        subExperimentsMeasurements(subExperimentId),
      ])
    );
    return { meta, measurements };
  },
});

export const filteredExperimentsDtoQuery = selectorFamily<ExperimentsWithMeanDto, SortParamsApi>({
  key: "FilteredExperimentsDtoQuery",
  get: (sortingParams: SortParamsApi) => async ({ get }) => {
    get(filteredExperimentsReqIdAtom);
    const projectId = get(projectSelectedIdAtom);
    const compoundId = get(compoundIdSelectedAtom);
    const targetId = get(targetIdSelectedAtom);
    return Api.fetchExperiments(projectId, compoundId, targetId, sortingParams);
  },
});

export const filteredExperimentsQuery = selectorFamily<ExperimentsWithMean, SortParamsApi>({
  key: "DashboardExperiments",
  get: (sortingParams: SortParamsApi) => ({ get }) => {
    const { experiments, meanIC50 } = get(filteredExperimentsDtoQuery(sortingParams));

    // TODO fix this quadratic time
    function findName(elements: { id: number; item: { name: string } }[], id: number) {
      return elements.find(x => x.id === id)?.item.name;
    }

    const projects = get(projectsQuery({}));
    const compounds = get(compoundsQuery({}));
    const targets = get(targetsQuery({}));
    const methodologies = get(methodologiesQuery({}));

    const exps: Experiment[] = experiments
      .map(e => {
        const projectName = findName(projects, e.item.project);
        const compoundName = findName(compounds, e.item.compound);
        const targetName = findName(targets, e.item.target);
        const methodologyName = e.item.methodology
          ? findName(methodologies, e.item.methodology)
          : undefined;
        return {
          id: e.id,
          projectName,
          compoundName,
          targetName,
          methodologyName,
          uploadDate: e.item.uploadDate,
          subExperiments: e.item.subExperiments,
          primarySubExperimentId: e.item.primarySubExperiment,
          primaryIC50: e.item.primaryIC50,
        };
      })
      .filter(
        e => isDefined(e.projectName) && isDefined(e.compoundName) && isDefined(e.targetName)
      ) as Experiment[];

    return {
      experiments: exps,
      meanIC50,
    };
  },
});

export const selectedSubExperimentsQuery = selector<SubExperimentWithMeasurements[]>({
  key: "SelectedSubExperiments",
  get: ({ get }) => {
    const ids = Array.from(get(selectedSubExperimentsIdsAtom).values());
    return get(waitForAll(ids.map(subId => subExperimentsWithMeasurementsQuery(subId))));
  },
});

export const selectedSubExperimentsIC50Query = selector<ResultDto<number>>({
  key: "SelectedSubExperimentsIC50",
  get: ({ get }) => {
    const subs = get(selectedSubExperimentsQuery);
    const ic50Sum = subs.reduce(
      (acc: ResultDto<number>, x) =>
        "Left" in acc
          ? acc
          : "Left" in x.meta.item.result
          ? { Left: x.meta.item.result.Left }
          : { Right: acc.Right + x.meta.item.result.Right[2] },
      { Right: 0 }
    );
    if ("Left" in ic50Sum) return ic50Sum;
    return { Right: ic50Sum.Right / subs.length };
  },
});

export const minAmountColorQuery = selector<string>({
  key: "MinAmountColor",
  get: ({ get }) => {
    const amounts = chartColors.map(col => get(colorsCounterAtom(col)));
    const minColorIdx = amounts.reduce((mnIdx, v, i) => (v < amounts[mnIdx] ? i : mnIdx), 0);
    return chartColors[minColorIdx];
  },
});
