import React from "react";
import EditSvg from "../../assets/svg/edit.svg";
import "./ContextActions.scss";
import DeleteSvg from "../../assets/svg/delete.svg";

interface ContextItemProps {
  onClick: () => void;
}

export function EditContextItem({ onClick }: ContextItemProps): React.ReactElement {
  return (
    <div key="edit" className="contextActions__item" onMouseDown={onClick}>
      <EditSvg />
      Edit
    </div>
  );
}

export function DeleteContextItem({ onClick }: ContextItemProps): React.ReactElement {
  return (
    <div key="delete" className="contextActions__item" onMouseDown={onClick}>
      <DeleteSvg />
      Delete
    </div>
  );
}
