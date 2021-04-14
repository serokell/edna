import { useRecoilValue, useSetRecoilState } from "recoil";
import React from "react";
import { modalDialogAtom } from "../../store/atoms";
import { projectsQuery } from "../../store/selectors";
import { ProjectDto } from "../../api/types";
import { extraFormatter, formatDateTimeDto } from "../../utils/utils";
import { ContextActions } from "../../components/ContextActions/ContextActions";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Button } from "../../components/Button/Button";
import { Table } from "../../components/Table/Table";
import { EditContextItem } from "../../components/ContextActions/ContextItems";

export function ProjectsSuspendable(): React.ReactElement {
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
              <EditContextItem
                key="edit"
                onClick={() =>
                  setModalDialog({
                    kind: "create-edit-project",
                    editing: p,
                  })
                }
              />,
            ]}
          />
        ),
      },
    ],
    [setModalDialog]
  );
  return projects.length === 0 ? (
    <EmptyPlaceholder
      title="No projects created yet"
      description="All created projects will be displayed here"
      button={
        <Button type="primary" onClick={() => setModalDialog({ kind: "create-edit-project" })}>
          Create project
        </Button>
      }
    />
  ) : (
    <Table data={projects} columns={projectColumns} />
  );
}
