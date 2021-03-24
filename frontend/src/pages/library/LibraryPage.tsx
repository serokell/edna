import React, { FunctionComponent, useState } from "react";
import { useRecoilValue } from "recoil";
import cx from "classnames";
import { Table } from "../../components/Table/Table";
import "./LibraryPage.scss";
import { compoundsAtom, methodologiesAtom, projectsAtom, targetsAtom } from "../../store/atoms";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CreateMethodologyButton } from "../../components/buttons/CreateMethodologyButton";
import { CreateProjectButton } from "../../components/buttons/CreateProjectButton";
import { CompoundDto, ProjectDto, TargetDto } from "../../api/types";
import { formatDateTimeDto } from "../../utils/utils";
import DotsSvg from "../../assets/svg/dots.svg";
import { MethodologyPlate } from "../../components/MethodologyPlate/MethodologyPlate";
import { ErrorPlaceholder } from "../../components/Error/ErrorPlaceholder";

export const LibraryPage: FunctionComponent = () => {
  return (
    <PageLayout>
      <EntitiesTab
        renderAddButton={activeTab => {
          if (activeTab === "project")
            return <CreateProjectButton className="libraryPage__addBtn" />;
          if (activeTab === "methodology")
            return <CreateMethodologyButton className="libraryPage__addBtn" />;
          return <></>;
        }}
        render={entity => {
          return (
            // TODO work out it's better
            <ErrorPlaceholder>
              <SuspenseSpinner>{suspendables[entity]()}</SuspenseSpinner>
            </ErrorPlaceholder>
          );
        }}
      />
    </PageLayout>
  );
};

const entities = {
  project: "Projects",
  compound: "Compounds",
  target: "Targets",
  methodology: "Methodologies",
};

const suspendables = {
  project: () => <ProjectsSuspendable />,
  compound: () => <CompoundsSuspendable />,
  target: () => <TargetsSuspendable />,
  methodology: () => <MethodsSuspendable />,
};

type EntityType = keyof typeof entities;

interface EntitiesTabProps {
  render: (entity: EntityType) => React.ReactNode;
  renderAddButton: (activeTab: EntityType) => React.ReactNode;
}

function EntitiesTab({ render, renderAddButton }: EntitiesTabProps) {
  const [entityTab, setEntityTab] = useState<EntityType>("project");

  return (
    <>
      <div className="entityTabs">
        {Object.entries(entities).map(([tpStr, title]) => (
          <div
            key={tpStr}
            className={cx("entityTabs__tab", entityTab === tpStr && "entityTabs__tab_active")}
            onClick={() => setEntityTab(tpStr as EntityType)}
          >
            {title}
          </div>
        ))}

        {renderAddButton(entityTab)}
      </div>
      {render(entityTab)}
    </>
  );
}

const extraFormatter = (compounds: string[]) => {
  if (compounds.length <= 4) return compounds.join(", ");
  return `${compounds.slice(0, 4).join(", ")} and ${compounds.length - 4} more`;
};

function ProjectsSuspendable() {
  const projects = useRecoilValue(projectsAtom);
  const projectColumns = React.useMemo(
    () => [
      {
        Header: "Project",
        accessor: (p: ProjectDto) => p.item.name,
      },
      {
        Header: "Compounds",
        accessor: (p: ProjectDto) => (
          <span className="project__compounds">{extraFormatter(p.item.compoundNames)}</span>
        ),
      },
      {
        Header: "Creation date",
        accessor: (p: ProjectDto) => formatDateTimeDto(p.item.creationDate),
      },
      {
        Header: "Last update",
        accessor: (p: ProjectDto) => formatDateTimeDto(p.item.lastUpdate),
      },
      {
        id: "actions",
        accessor: () => (
          <div className="contextActions">
            <DotsSvg />
          </div>
        ),
      },
    ],
    []
  );
  return <Table mode="bordered" data={projects} columns={projectColumns} />;
}

function CompoundsSuspendable() {
  const projects = useRecoilValue(compoundsAtom);
  const projectColumns = React.useMemo(
    () => [
      {
        Header: "Compounds",
        accessor: (c: CompoundDto) => c.item.name,
      },
      {
        Header: "MDe link",
        // TODO form MDe link
        accessor: (c: CompoundDto) => <a href={c.item.name}>mde.io</a>,
      },
      {
        id: "actions",
        accessor: () => (
          <div className="contextActions">
            <DotsSvg />
          </div>
        ),
      },
    ],
    []
  );
  return <Table mode="bordered" data={projects} columns={projectColumns} />;
}

function TargetsSuspendable() {
  const projects = useRecoilValue(targetsAtom);
  const projectColumns = React.useMemo(
    () => [
      {
        Header: "Target",
        accessor: (t: TargetDto) => t.item.name,
      },
      {
        Header: "Projects",
        accessor: (t: TargetDto) => extraFormatter(t.item.projects),
      },
      {
        Header: "Addition date",
        accessor: (t: TargetDto) => formatDateTimeDto(t.item.additionDate),
      },
      {
        id: "actions",
        accessor: () => (
          <div className="contextActions">
            <DotsSvg />
          </div>
        ),
      },
    ],
    []
  );
  return <Table mode="bordered" data={projects} columns={projectColumns} />;
}

function MethodsSuspendable() {
  const methodologies = useRecoilValue(methodologiesAtom);
  return (
    <>
      {methodologies.map(m => (
        <MethodologyPlate key={m.item.name} title={m.item.name} description={m.item?.description} />
      ))}
    </>
  );
}
