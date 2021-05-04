// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { useCallback } from "react";
import { useRecoilValueLoadable, useSetRecoilState, waitForAll } from "recoil";
import { Experiment } from "store/types";
import { DescriptiveSelector } from "../../components/DescriptiveSelector/DescriptiveSelector";
import {
  compoundSelectedQuery,
  compoundsQuery,
  projectSelectedQuery,
  projectsQuery,
  selectedTargetIC50Query,
  targetSelectedQuery,
  targetsQuery,
} from "../../store/selectors";
import {
  compoundIdSelectedAtom,
  modalDialogAtom,
  projectSelectedIdAtom,
  targetIdSelectedAtom,
} from "../../store/atoms";
import { CompoundDto, ProjectDto, TargetDto } from "../../api/types";
import { formatAsDate, isDefined, Maybe } from "../../utils/utils";
import { Button } from "../../components/Button/Button";
import { ContextItem } from "../../components/ContextActions/ContextItems";
import { IC50Tooltip } from "../../components/IC50Line/IC50Tooltip";
import "../../components/DescriptiveSelector/DescriptivePlate.scss";

interface SelectorProps {
  className?: string;
  experiments?: Experiment[];
}

const projectsFilter = (
  compound: Maybe<CompoundDto>,
  target: Maybe<TargetDto>,
  experimentsF: Experiment[]
): Set<string> =>
  new Set(
    experimentsF
      .filter(
        experiment =>
          (!isDefined(compound) || experiment.compoundName === compound.item.name) &&
          (!isDefined(target) || experiment.targetName === target.item.name)
      )
      .map(experiment => experiment.projectName)
  );

const compoundsFilter = (
  project: Maybe<ProjectDto>,
  target: Maybe<TargetDto>,
  experimentsF: Experiment[]
): Set<string> =>
  new Set(
    experimentsF
      .filter(
        experiment =>
          (!isDefined(project) || experiment.projectName === project.item.name) &&
          (!isDefined(target) || experiment.targetName === target.item.name)
      )
      .map(experiment => experiment.compoundName)
  );

const targetsFilter = (
  project: Maybe<ProjectDto>,
  compound: Maybe<CompoundDto>,
  experimentsF: Experiment[]
): Set<string> =>
  new Set(
    experimentsF
      .filter(
        experiment =>
          (!isDefined(project) || experiment.projectName === project.item.name) &&
          (!isDefined(compound) || experiment.compoundName === compound.item.name)
      )
      .map(experiment => experiment.targetName)
  );

export function ProjectSelector({ className, experiments }: SelectorProps): React.ReactElement {
  const filterNeeds = useRecoilValueLoadable(
    waitForAll([compoundSelectedQuery, targetSelectedQuery])
  );
  // TODO make it async one day?
  const projectsL = useRecoilValueLoadable(projectsQuery({}));
  const projectSelectedL = useRecoilValueLoadable(projectSelectedQuery);
  const setProjectSelected = useSetRecoilState(projectSelectedIdAtom);
  const setCompoundSelected = useSetRecoilState(compoundIdSelectedAtom);
  const setTargetSelected = useSetRecoilState(targetIdSelectedAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  const projectsConstruct = useCallback(
    (projects: ProjectDto[]) => {
      if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
        const [selectedCompound, selectedTarget] = filterNeeds.contents;
        const projectNames = projectsFilter(selectedCompound, selectedTarget, experiments);

        const [filteredProjects, restProjects] = projects.reduce(
          ([filteredProject, restProject]: ProjectDto[][], project) => {
            if (projectNames.has(project.item.name)) {
              filteredProject.push(project);
            } else {
              restProject.push(project);
            }
            return [filteredProject, restProject];
          },
          [[], []]
        );

        if (restProjects.length !== 0) {
          filteredProjects.push({
            id: -1,
            item: {
              name: "Start new filter with:",
              creationDate: "",
              lastUpdate: "",
              compoundNames: [],
            },
          });
        }

        return filteredProjects.concat(restProjects);
      }
      return undefined;
    },
    [filterNeeds, experiments]
  );

  return (
    <DescriptiveSelector<ProjectDto>
      className={className}
      value={projectSelectedL.state === "hasValue" ? projectSelectedL.contents : undefined}
      onChange={x => {
        if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
          const [selectedCompound, selectedTarget] = filterNeeds.contents;
          const targetNames = targetsFilter(x, selectedCompound, experiments);
          const compoundNames = compoundsFilter(x, selectedTarget, experiments);
          if (isDefined(selectedCompound) && !compoundNames.has(selectedCompound.item.name)) {
            setCompoundSelected(undefined);
          }
          if (isDefined(selectedTarget) && !targetNames.has(selectedTarget.item.name)) {
            setTargetSelected(undefined);
          }
        }
        setProjectSelected(x?.id);
      }}
      optionsLoadable={projectsL}
      placeholder="Select a project"
      placeholderEmpty="No projects"
      toEntityProperties={p =>
        p.item.description
          ? [
              { label: "Last updated:", value: formatAsDate(p.item.lastUpdate) },
              { label: "Description:", value: p.item.description },
            ]
          : [{ label: "Last updated:", value: formatAsDate(p.item.lastUpdate) }]
      }
      toOption={proj => ({
        value: `${proj.id}`,
        label: proj.item.name,
        isDisabled: proj.id === -1,
      })}
      contextActions={[
        <ContextItem
          type="edit"
          key="edit"
          onClick={() => {
            if (projectSelectedL.state === "hasValue" && projectSelectedL.contents) {
              setModalDialog({
                kind: "create-edit-project",
                editing: projectSelectedL.contents,
              });
            }
          }}
        />,
      ]}
      optionsFilter={projectsConstruct}
    />
  );
}

export function CompoundSelector({ className, experiments }: SelectorProps): React.ReactElement {
  const filterNeeds = useRecoilValueLoadable(
    waitForAll([projectSelectedQuery, targetSelectedQuery])
  );
  // TODO make it async one day?
  const compoundsL = useRecoilValueLoadable(compoundsQuery({}));
  const compoundSelectedL = useRecoilValueLoadable(compoundSelectedQuery);
  const setProjectSelected = useSetRecoilState(projectSelectedIdAtom);
  const setCompoundSelected = useSetRecoilState(compoundIdSelectedAtom);
  const setTargetSelected = useSetRecoilState(targetIdSelectedAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  const compoundsConstruct = useCallback(
    (compounds: CompoundDto[]) => {
      if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
        const [selectedProject, selectedTarget] = filterNeeds.contents;
        const compoundNames = compoundsFilter(selectedProject, selectedTarget, experiments);

        const [filteredCompounds, restCompounds] = compounds.reduce(
          (acc: CompoundDto[][], compound) => {
            acc[compoundNames.has(compound.item.name) ? 0 : 1].push(compound);
            return acc;
          },
          [[], []]
        );

        if (restCompounds.length !== 0) {
          filteredCompounds.push({
            id: -1,
            item: {
              name: "Start new filter with:",
              additionDate: "",
            },
          });
        }

        return filteredCompounds.concat(restCompounds);
      }
      return undefined;
    },
    [filterNeeds, experiments]
  );

  return (
    <DescriptiveSelector<CompoundDto>
      className={className}
      isLoading={compoundSelectedL.state === "loading"}
      value={compoundSelectedL.state === "hasValue" ? compoundSelectedL.contents : undefined}
      onChange={x => {
        if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
          const [selectedProject, selectedTarget] = filterNeeds.contents;
          const targetNames = targetsFilter(selectedProject, x, experiments);
          const projectNames = projectsFilter(x, selectedTarget, experiments);
          if (isDefined(selectedProject) && !projectNames.has(selectedProject.item.name)) {
            setProjectSelected(undefined);
          }
          if (isDefined(selectedTarget) && !targetNames.has(selectedTarget.item.name)) {
            setTargetSelected(undefined);
          }
        }
        setCompoundSelected(x?.id);
      }}
      optionsLoadable={compoundsL}
      placeholder="Select a compound"
      placeholderEmpty="No compounds"
      toEntityProperties={c => {
        const chemsoft = c.item.chemSoft ? (
          <a href={c.item.chemSoft}>{c.item.chemSoft}</a>
        ) : (
          <Button
            type="half-rounded"
            size="small"
            onClick={() => {
              setModalDialog({
                kind: "add-edit-link",
                target: { kind: "compound-chemsoft", object: c },
              });
            }}
          >
            Add link
          </Button>
        );

        const mde = c.item.mde ? (
          <a href={c.item.mde}>{c.item.mde}</a>
        ) : (
          <Button
            type="half-rounded"
            size="small"
            onClick={() => {
              setModalDialog({
                kind: "add-edit-link",
                target: { kind: "compound-mde", object: c },
              });
            }}
          >
            Add link
          </Button>
        );
        return [
          { label: "Chemsoft:", value: chemsoft },
          { label: "MDe link:", value: mde },
        ];
      }}
      toOption={c => ({ value: `${c.id}`, label: c.item.name, isDisabled: c.id === -1 })}
      contextActions={[
        <ContextItem
          type="edit"
          key="edit-chemsoft"
          value="Edit ChemSoft"
          onClick={() => {
            if (compoundSelectedL.state === "hasValue" && compoundSelectedL.contents) {
              setModalDialog({
                kind: "add-edit-link",
                target: { kind: "compound-chemsoft", object: compoundSelectedL.contents },
              });
            }
          }}
        />,
        <ContextItem
          type="edit"
          key="edit-mde"
          value="Edit Mde"
          onClick={() => {
            if (compoundSelectedL.state === "hasValue" && compoundSelectedL.contents) {
              setModalDialog({
                kind: "add-edit-link",
                target: { kind: "compound-mde", object: compoundSelectedL.contents },
              });
            }
          }}
        />,
      ]}
      optionsFilter={compoundsConstruct}
    />
  );
}

export function TargetSelector({ className, experiments }: SelectorProps): React.ReactElement {
  const filterNeeds = useRecoilValueLoadable(
    waitForAll([compoundSelectedQuery, projectSelectedQuery])
  );
  const targetsLoadable = useRecoilValueLoadable(targetsQuery({}));
  const targetSelected = useRecoilValueLoadable(targetSelectedQuery);
  const setProjectSelected = useSetRecoilState(projectSelectedIdAtom);
  const setCompoundSelected = useSetRecoilState(compoundIdSelectedAtom);
  const setTargetSelected = useSetRecoilState(targetIdSelectedAtom);

  const targetsConstruct = useCallback(
    (targets: TargetDto[]) => {
      if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
        const [selectedCompound, selectedProject] = filterNeeds.contents;
        const targetNames = targetsFilter(selectedProject, selectedCompound, experiments);

        const [filteredTargets, restTargets] = targets.reduce(
          (acc: TargetDto[][], target) => {
            acc[targetNames.has(target.item.name) ? 0 : 1].push(target);
            return acc;
          },
          [[], []]
        );

        if (restTargets.length !== 0) {
          filteredTargets.push({
            id: -1,
            item: {
              name: "Start new filter with:",
              projects: [],
              additionDate: "",
            },
          });
        }

        return filteredTargets.concat(restTargets);
      }
      return undefined;
    },
    [filterNeeds, experiments]
  );

  const compoundSelected = useRecoilValueLoadable(compoundSelectedQuery);
  const targetIC50 = useRecoilValueLoadable(selectedTargetIC50Query);
  return (
    <DescriptiveSelector<TargetDto>
      className={className}
      value={targetSelected.state === "hasValue" ? targetSelected.contents : undefined}
      onChange={x => {
        if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
          const [selectedCompound, selectedProject] = filterNeeds.contents;
          const compoundNames = compoundsFilter(selectedProject, x, experiments);
          const projectNames = projectsFilter(selectedCompound, x, experiments);
          if (isDefined(selectedProject) && !projectNames.has(selectedProject.item.name)) {
            setProjectSelected(undefined);
          }
          if (isDefined(selectedCompound) && !compoundNames.has(selectedCompound.item.name)) {
            setCompoundSelected(undefined);
          }
        }
        setTargetSelected(x?.id);
      }}
      optionsLoadable={targetsLoadable}
      toEntityProperties={t => {
        if (
          compoundSelected.state === "hasValue" &&
          targetSelected.state === "hasValue" &&
          targetIC50.state === "hasValue" &&
          isDefined(compoundSelected.contents) &&
          isDefined(targetSelected.contents) &&
          isDefined(targetIC50.contents)
        ) {
          return [
            { label: "Added:", value: formatAsDate(t.item.additionDate) },
            {
              label: "Mean IC50:",
              value: (
                <span>
                  <IC50Tooltip ic50={targetIC50.contents} />
                  <span className="descriptivePlate__label">{` (${compoundSelected.contents.item.name} ⟶ ${targetSelected.contents.item.name})`}</span>
                </span>
              ),
            },
          ];
        }
        return [{ label: "Added:", value: formatAsDate(t.item.additionDate) }];
      }}
      placeholder="Select a target"
      placeholderEmpty="No targets"
      toOption={c => ({ value: `${c.id}`, label: c.item.name, isDisabled: c.id === -1 })}
      optionsFilter={targetsConstruct}
    />
  );
}
