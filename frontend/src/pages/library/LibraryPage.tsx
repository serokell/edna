import React, { FunctionComponent, useState } from "react";
import { useRecoilValue } from "recoil";
import cx from "classnames";
import Header from "../../components/Header/Header";
import { LibraryTable } from "./LibraryTable";
import "./LibraryPage.scss";
import { methodologiesAtom, projectsAtom } from "../../store/atoms";
import { SuspenseSpinner } from "../../components/SuspsenseSpinner";

export const LibraryPage: FunctionComponent = () => {
  return (
    <>
      <Header />
      <div className="container">
        <EntitiesTab
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
      </div>
    </>
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
}

function EntitiesTab({ render }: EntitiesTabProps) {
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
  return <LibraryTable data={projects} columns={projectColumns} />;
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
  return <LibraryTable data={methodologies} columns={methodologiesColumns} />;
}
