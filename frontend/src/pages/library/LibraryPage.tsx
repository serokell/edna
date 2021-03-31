import React, { FunctionComponent, useState } from "react";
import { useRecoilValue, useSetRecoilState } from "recoil";
import cx from "classnames";
import { Column } from "react-table";
import { Table } from "../../components/Table/Table";
import "./LibraryPage.scss";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CreateMethodologyButton } from "../../components/buttons/CreateMethodologyButton";
import { CreateProjectButton } from "../../components/buttons/CreateProjectButton";
import { CompoundDto, MethodologyDto, ProjectDto, TargetDto } from "../../api/types";
import { formatDateTimeDto } from "../../utils/utils";
import { ErrorPlaceholder } from "../../components/Error/ErrorPlaceholder";
import {
  compoundsQuery,
  methodologiesQuery,
  projectsQuery,
  targetsQuery,
} from "../../store/selectors";
import { ContextActions } from "../../components/ContextActions/ContextActions";
import EditSvg from "../../assets/svg/edit.svg";
import DeleteSvg from "../../assets/svg/delete.svg";
import { Button } from "../../components/Button/Button";
import { modalDialogAtom } from "../../store/atoms";

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
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const projects = useRecoilValue(projectsQuery);
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
        accessor: (p: ProjectDto) => (
          <ContextActions
            actions={[
              <div
                key="edit"
                className="contextActions__item"
                onClick={() =>
                  setModalDialog({
                    kind: "create-edit-project",
                    editing: p,
                  })
                }
              >
                <EditSvg />
                Edit
              </div>,
            ]}
          />
        ),
      },
    ],
    [setModalDialog]
  );
  return <Table data={projects} columns={projectColumns} />;
}

function CompoundsSuspendable() {
  const projects = useRecoilValue(compoundsQuery);
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const projectColumns = React.useMemo(
    () => [
      {
        Header: "Compounds",
        accessor: (c: CompoundDto) => c.item.name,
      },
      {
        Header: "MDe link",
        // TODO form MDe link
        accessor: (c: CompoundDto) => (
          <a className="cellText" href={c.item.name}>
            mde.io
          </a>
        ),
      },
      {
        Header: "ChemSoft link",
        id: "chemsoft",
        accessor: (c: CompoundDto) => {
          if (c.item.chemSoft)
            return (
              <td className="ednaTable__cell">
                <a href={c.item.chemSoft}>{c.item.chemSoft}</a>
              </td>
            );
          return (
            <td className="ednaTable__cell cellBtn">
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
            </td>
          );
        },
      },
      {
        id: "actions",
        accessor: (c: CompoundDto) => (
          <ContextActions
            actions={[
              <div
                key="edit"
                className="contextActions__item"
                onClick={() => {
                  setModalDialog({
                    kind: "add-edit-link",
                    target: { kind: "compound", object: c },
                  });
                }}
              >
                <EditSvg />
                Edit
              </div>,
            ]}
          />
        ),
      },
    ],
    [setModalDialog]
  );
  return (
    <Table
      data={projects}
      columns={projectColumns}
      columnExtras={{
        chemsoft: { manualCellRendering: true },
      }}
    />
  );
}

function TargetsSuspendable() {
  const projects = useRecoilValue(targetsQuery);
  const projectColumns: Column<TargetDto>[] = React.useMemo(
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
    ],
    []
  );
  return <Table data={projects} columns={projectColumns} />;
}

function MethodsSuspendable() {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const methodologies = useRecoilValue(methodologiesQuery);
  const methodologyColumns: Column<MethodologyDto>[] = React.useMemo(
    () => [
      {
        Header: "Methodology",
        accessor: (t: MethodologyDto) => t.item.name,
      },
      {
        Header: "Project",
        accessor: () => "project1, project2",
      },
      {
        Header: "Confluence link",
        id: "link",
        accessor: (m: MethodologyDto) => {
          if (m.item.confluence)
            return (
              <td className="ednaTable__cell">
                <a href={m.item.confluence}>{m.item.confluence}</a>
              </td>
            );
          return (
            <td className="ednaTable__cell cellBtn">
              <Button
                type="half-rounded"
                size="small"
                onClick={() => {
                  setModalDialog({
                    kind: "add-edit-link",
                    target: { kind: "methodology", object: m },
                  });
                }}
              >
                Add link
              </Button>
            </td>
          );
        },
      },
      {
        Header: "Description",
        id: "description",
        accessor: (m: MethodologyDto) => {
          return (
            <td className="ednaTable__cell cellBtn">
              <Button
                type="half-rounded"
                size="small"
                onClick={() => {
                  setModalDialog({
                    kind: "methodology-description",
                    methodology: m,
                  });
                }}
              >
                Show description
              </Button>
            </td>
          );
        },
      },

      {
        id: "actions",
        accessor: (m: MethodologyDto) => (
          <ContextActions
            actions={[
              <div
                key="edit"
                className="contextActions__item"
                onClick={() => {
                  setModalDialog({
                    kind: "create-edit-methodology",
                    editing: m,
                  });
                }}
              >
                <EditSvg />
                Edit
              </div>,
              <div
                key="delete"
                className="contextActions__item"
                onClick={() => {
                  setModalDialog({
                    kind: "delete-methodology",
                    methodology: m,
                  });
                }}
              >
                <DeleteSvg />
                Delete
              </div>,
            ]}
          />
        ),
      },
    ],
    [setModalDialog]
  );

  return (
    <Table
      data={methodologies}
      columns={methodologyColumns}
      columnExtras={{
        link: { manualCellRendering: true },
        description: { manualCellRendering: true },
      }}
    />
  );
}
