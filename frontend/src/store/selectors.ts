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
  subExperimentsMeasurementsMap,
  subExperimentsMetaMap,
  targetIdSelectedAtom,
  targetsRequestIdAtom,
} from "./atoms";
import Api from "../api/api";
import { evalMeanIC50, isDefined, Maybe } from "../utils/utils";
import {
  CompoundDto,
  ExperimentsWithMeanDto,
  ProjectDto,
  ResultDto,
  SubExperimentDto,
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

export const subExperimentWithMeasurementsMap = selectorFamily<
  SubExperimentWithMeasurements,
  number
>({
  key: "SubExperiments",
  get: subExperimentId => async ({ get }) => {
    const [meta, measurements] = get(
      waitForAll([
        subExperimentsMetaMap(subExperimentId),
        subExperimentsMeasurementsMap(subExperimentId),
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
    return get(waitForAll(ids.map(subId => subExperimentWithMeasurementsMap(subId))));
  },
});

export const selectedSubExperimentsMetaQuery = selector<SubExperimentDto[]>({
  key: "SelectedSubExperimentsMeta",
  get: ({ get }) => {
    const ids = Array.from(get(selectedSubExperimentsIdsAtom).values());
    return get(waitForAll(ids.map(subId => subExperimentsMetaMap(subId))));
  },
});

export const selectedSubExperimentsIC50Query = selector<ResultDto<number>>({
  key: "SelectedSubExperimentsIC50",
  get: ({ get }) => {
    const subs = get(selectedSubExperimentsMetaQuery);
    const exps = get(filteredExperimentsQuery({})).experiments;
    const avgs = exps.reduce((acc: ResultDto<number>[], e) => {
      const exSubs = subs.filter(s => e.subExperiments.find(se => s.id === se));
      if (exSubs.length > 0) {
        return acc.concat([evalMeanIC50(exSubs.map(x => x.item.result))]);
      }
      return acc;
    }, []);
    return evalMeanIC50(avgs);
  },
});

export const selectedTargetIC50Query = selector<Maybe<ResultDto<number>>>({
  key: "SelectedTargetIC50",
  get: ({ get }) => {
    const compound = get(compoundSelectedQuery);
    const target = get(targetSelectedQuery);
    if (!isDefined(compound) || !isDefined(target)) {
      // Dumb value for undefined case
      return undefined;
    }
    const ic50s = get(filteredExperimentsQuery({})).experiments.map(x => x.primaryIC50);
    if (ic50s.length === 0) {
      return undefined;
    }
    return evalMeanIC50(ic50s);
  },
});

export interface SubExperimentWithMeasurementsExtra extends SubExperimentWithMeasurements {
  compound: string;
  target: string;
}

export const selectedSubExperimentsExtraQuery = selector<SubExperimentWithMeasurementsExtra[]>({
  key: "SelectedSubExperiments",
  get: ({ get }) => {
    const ids = Array.from(get(selectedSubExperimentsIdsAtom).values());
    const experiments = Array.from(get(filteredExperimentsQuery({})).experiments);
    return get(waitForAll(ids.map(subId => subExperimentWithMeasurementsMap(subId)))).map(sex => {
      const experiment = experiments.find(e => e.subExperiments.find(v => v === sex.meta.id));
      return { compound: experiment!.compoundName, target: experiment!.targetName, ...sex };
    });
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
