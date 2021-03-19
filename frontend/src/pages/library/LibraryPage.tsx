import React, { FunctionComponent, useState } from "react";
import { useRecoilValue } from "recoil";
import cx from "classnames";
import { Table } from "../../components/Table/Table";
import "./LibraryPage.scss";
import { methodologiesAtom, projectsAtom } from "../../store/atoms";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CreateMethodologyButton } from "../../components/buttons/CreateMethodologyButton";
import { CreateProjectButton } from "../../components/buttons/CreateProjectButton";
import { ProjectDto } from "../../api/types";
import { formatTimestamp } from "../../utils/utils";
import DotsSvg from "../../assets/svg/dots.svg";

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
          if (entity === "project") {
            return (
              <SuspenseSpinner>
                <ProjectsSuspendable />
              </SuspenseSpinner>
            );
          }
          if (entity === "methodology") {
            return (
              <SuspenseSpinner>
                <MethodsSuspendable />
              </SuspenseSpinner>
            );
          }
          return <>Not implemented yet</>;
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

function ProjectsSuspendable() {
  const formatCompounds = (compounds: string[]) => {
    if (compounds.length <= 4) return compounds.join(", ");
    return `${compounds.slice(0, 4).join(", ")} and ${compounds.length - 4} more`;
  };

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
          <span className="project__compounds">{formatCompounds(p.extra.compoundNames)}</span>
        ),
      },
      {
        Header: "Creation date",
        accessor: (p: ProjectDto) => formatTimestamp(p.extra.creationDate),
      },
      {
        Header: "Last update",
        accessor: (p: ProjectDto) => formatTimestamp(p.extra.lastUpdate),
      },
      {
        id: "actions",
        accessor: () => (
          <div className="project__actions">
            <DotsSvg />{" "}
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
  const methodologiesColumns = React.useMemo(
    () => [
      {
        Header: "Methodology",
        accessor: "name" as const, // accessor is the "key" in the data
      },
    ],
    []
  );
  return <Table mode="bordered" data={methodologies} columns={methodologiesColumns} />;
}
