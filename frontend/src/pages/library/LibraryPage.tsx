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
  const projects = useRecoilValue(projectsAtom);
  const projectColumns = React.useMemo(
    () => [
      {
        Header: "Project",
        accessor: "name" as const, // accessor is the "key" in the data
      },
      {
        Header: "Description",
        accessor: "description" as const,
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
