import { useRecoilValue, useSetRecoilState } from "recoil";
import { Column } from "react-table";
import React from "react";
import { modalDialogAtom } from "../../store/atoms";
import { methodologiesQuery } from "../../store/selectors";
import { MethodologyDto } from "../../api/types";
import { Button } from "../../components/Button/Button";
import { ContextActions } from "../../components/ContextActions/ContextActions";
import EditSvg from "../../assets/svg/edit.svg";
import DeleteSvg from "../../assets/svg/delete.svg";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Table } from "../../components/Table/Table";

export function MethodsSuspendable(): React.ReactElement {
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
            <td className="ednaTable__cell libraryTable__cellBtn">
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
                onMouseDown={() => {
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
                onMouseDown={() => {
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

  return methodologies.length === 0 ? (
    <EmptyPlaceholder
      title="No methodologies created yet"
      description="All created methodologies will be displayed here"
      button={
        <Button type="primary" onClick={() => setModalDialog({ kind: "create-edit-methodology" })}>
          Create methodology
        </Button>
      }
    />
  ) : (
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
