// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { useRecoilValueLoadable, useSetRecoilState } from "recoil";
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
import { formatAsDate } from "../../utils/utils";
import { Button } from "../../components/Button/Button";
import { EntityProperty } from "../../components/DescriptiveSelector/DescriptivePlate";
import { ContextItem } from "../../components/ContextActions/ContextItems";

interface SelectorProps {
  className?: string;
}

export function ProjectSelector({ className }: SelectorProps): React.ReactElement {
  const projectsL = useRecoilValueLoadable(projectsQuery);
  const projectSelectedL = useRecoilValueLoadable(projectSelectedQuery);
  const setProjectSelected = useSetRecoilState(projectSelectedIdAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);

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
    />
  );
}

export function CompoundSelector({ className }: SelectorProps): React.ReactElement {
  const compoundsL = useRecoilValueLoadable(compoundsQuery);
  const compoundSelectedL = useRecoilValueLoadable(compoundSelectedQuery);
  const setCompoundSelected = useSetRecoilState(compoundIdSelectedAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);

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
        const properties: EntityProperty[] = [{ label: "MDe link:", value: <a href="mde">mde</a> }];
        let chemsoftValue: React.ReactNode;

        if (c.item.chemSoft) chemsoftValue = <a href={c.item.chemSoft}>{c.item.chemSoft}</a>;
        else
          chemsoftValue = (
            <Button
              type="half-rounded"
              size="small"
              onClick={() => {
                setModalDialog({
                  kind: "add-edit-link",
                  target: { kind: "compound", object: c },
                });
              }}
            >
              Add link
            </Button>
          );
        properties.push({ label: "Chemsoft:", value: chemsoftValue });
        return properties;
      }}
      toOption={c => ({ value: `${c.id}`, label: c.item.name })}
      contextActions={[
        <ContextItem
          type="edit"
          key="edit"
          onClick={() => {
            if (compoundSelectedL.state === "hasValue" && compoundSelectedL.contents) {
              setModalDialog({
                kind: "add-edit-link",
                target: { kind: "compound", object: compoundSelectedL.contents },
              });
            }
          }}
        />,
      ]}
    />
  );
}

export function TargetSelector({ className }: SelectorProps): React.ReactElement {
  const targetsLoadable = useRecoilValueLoadable(targetsQuery);
  const targetSelected = useRecoilValueLoadable(targetSelectedQuery);
  const setTargetSelected = useSetRecoilState(targetIdSelectedAtom);
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
    />
  );
}
