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
import { formatAsDate, isDefined } from "../../utils/utils";
import { Button } from "../../components/Button/Button";
import { ContextItem } from "../../components/ContextActions/ContextItems";

interface SelectorProps {
  className?: string;
  experiments?: Experiment[];
}

export function ProjectSelector({ className, experiments }: SelectorProps): React.ReactElement {
  const filterNeeds = useRecoilValueLoadable(
    waitForAll([compoundSelectedQuery, targetSelectedQuery])
  );
  // TODO make it async one day?
  const projectsL = useRecoilValueLoadable(projectsQuery({}));
  const projectSelectedL = useRecoilValueLoadable(projectSelectedQuery);
  const setProjectSelected = useSetRecoilState(projectSelectedIdAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  const projectsFilter = useCallback(
    (projects: ProjectDto[]) => {
      if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
        const [selectedCompound, selectedTarget] = filterNeeds.contents;
        const projectNames = experiments
          .filter(
            experiment =>
              (isDefined(selectedCompound)
                ? experiment.compoundName === selectedCompound.item.name
                : true) &&
              (isDefined(selectedTarget)
                ? experiment.targetName === selectedTarget.item.name
                : true)
          )
          .map(experiment => experiment.projectName);

        return projects.filter(
          project => !!projectNames.find(projectName => projectName === project.item.name)
        );
      }
      return undefined;
    },
    [filterNeeds, experiments]
  );

  return (
    <DescriptiveSelector<ProjectDto>
      className={className}
      value={projectSelectedL.state === "hasValue" ? projectSelectedL.contents : undefined}
      onChange={x => setProjectSelected(x?.id)}
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
      toOption={proj => ({ value: `${proj.id}`, label: proj.item.name })}
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
      optionsFilter={projectsFilter}
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
  const setCompoundSelected = useSetRecoilState(compoundIdSelectedAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  const compoundsFilter = useCallback(
    (compounds: CompoundDto[]) => {
      if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
        const [selectedProject, selectedTarget] = filterNeeds.contents;
        const compoundNames = experiments
          .filter(
            experiment =>
              (isDefined(selectedProject)
                ? experiment.projectName === selectedProject.item.name
                : true) &&
              (isDefined(selectedTarget)
                ? experiment.targetName === selectedTarget.item.name
                : true)
          )
          .map(experiment => experiment.compoundName);

        return compounds.filter(
          compound => !!compoundNames.find(compoundName => compoundName === compound.item.name)
        );
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
      onChange={x => setCompoundSelected(x?.id)}
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
      toOption={c => ({ value: `${c.id}`, label: c.item.name })}
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
      optionsFilter={compoundsFilter}
    />
  );
}

export function TargetSelector({ className, experiments }: SelectorProps): React.ReactElement {
  const filterNeeds = useRecoilValueLoadable(
    waitForAll([compoundSelectedQuery, projectSelectedQuery])
  );
  const targetsLoadable = useRecoilValueLoadable(targetsQuery({}));
  const targetSelected = useRecoilValueLoadable(targetSelectedQuery);
  const setTargetSelected = useSetRecoilState(targetIdSelectedAtom);

  const targetsFilter = useCallback(
    (targets: TargetDto[]) => {
      if (filterNeeds.state === "hasValue" && filterNeeds.contents && experiments) {
        const [selectedCompound, selectedProject] = filterNeeds.contents;
        const targetNames = experiments
          .filter(
            experiment =>
              (isDefined(selectedProject)
                ? experiment.projectName === selectedProject.item.name
                : true) &&
              (isDefined(selectedCompound)
                ? experiment.compoundName === selectedCompound.item.name
                : true)
          )
          .map(experiment => experiment.targetName);

        return targets.filter(
          target => !!targetNames.find(targetName => targetName === target.item.name)
        );
      }
      return undefined;
    },
    [filterNeeds, experiments]
  );

  return (
    <DescriptiveSelector<TargetDto>
      className={className}
      value={targetSelected.state === "hasValue" ? targetSelected.contents : undefined}
      onChange={x => setTargetSelected(x?.id)}
      optionsLoadable={targetsLoadable}
      toEntityProperties={t => [{ label: "Added:", value: formatAsDate(t.item.additionDate) }]}
      placeholder="Select a target"
      placeholderEmpty="No targets"
      toOption={c => ({ value: `${c.id}`, label: c.item.name })}
      optionsFilter={targetsFilter}
    />
  );
}
