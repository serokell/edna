// Derived state should be placed here
import { selector, selectorFamily, waitForAll } from "recoil";
import {
  compoundIdSelectedAtom,
  compoundsReqIdAtom,
  methodologiesRequestIdAtom,
  projectSelectedIdAtom,
  projectsRequestIdAtom,
  selectedSubExperimentsIdsAtom,
  subExperimentsMeasurements,
  subExperimentsMeta,
  targetIdSelectedAtom,
  targetsRequestIdAtom,
} from "./atoms";
import Api from "../api/api";
import { isDefined, Maybe } from "../utils/utils";
import { CompoundDto, ExperimentsWithMeanDto, ProjectDto, TargetDto } from "../api/types";
import { Experiment, ExperimentsWithMean, SubExperimentWithMeasurements } from "./types";

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

export const subExperimentsWithMeasurementsQuery = selectorFamily<
  SubExperimentWithMeasurements,
  number
>({
  key: "SubExperiments",
  get: subExperimentId => async ({ get }) => {
    const [meta, measurements] = get(
      waitForAll([subExperimentsMeta(subExperimentId), subExperimentsMeasurements(subExperimentId)])
    );
    return { meta, measurements };
  },
});

const filteredExperimentsDtoQuery = selector<ExperimentsWithMeanDto>({
  key: "FilteredExperimentsDtoQuery",
  get: async ({ get }) => {
    const projectId = get(projectSelectedIdAtom);
    const compoundId = get(compoundIdSelectedAtom);
    const targetId = get(targetIdSelectedAtom);
    return Api.fetchExperiments(projectId, compoundId, targetId);
  },
});

export const filteredExperimentsQuery = selector<ExperimentsWithMean>({
  key: "DashboardExperiments",
  get: ({ get }) => {
    const { experiments, meanIC50 } = get(filteredExperimentsDtoQuery);

    // TODO fix this quadratic time
    function findName(elements: { id: number; item: { name: string } }[], id: number) {
      return elements.find(x => x.id === id)?.item.name;
    }

    const projects = get(projectsQuery);
    const compounds = get(compoundsQuery);
    const targets = get(targetsQuery);
    const methodologies = get(methodologiesQuery);

    const exps: Experiment[] = experiments
      .map(e => {
        const projectName = findName(projects, e.item.project);
        const compoundName = findName(compounds, e.item.compound);
        const targetName = findName(targets, e.item.target);
        const methodologyName = findName(methodologies, e.item.methodology);
        return {
          id: e.id,
          projectName,
          compoundName,
          targetName,
          methodologyName,
          uploadDate: e.item.uploadDate,
          subExperiments: e.item.subExperiments,
          primarySubExperiment: get(subExperimentsMeta(e.item.primarySubExperiment)),
        };
      })
      .filter(
        e =>
          isDefined(e.projectName) &&
          isDefined(e.compoundName) &&
          isDefined(e.targetName) &&
          isDefined(e.methodologyName)
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

export const selectedExperimentsQuery = selector<Set<number>>({
  key: "SelectedExperiments",
  get: ({ get }) => {
    const subs = get(selectedSubExperimentsIdsAtom);
    const filtered = get(filteredExperimentsQuery).experiments;
    return new Set(
      filtered.filter(e => isDefined(e.subExperiments.find(x => subs.has(x)))).map(x => x.id)
    );
  },
});
