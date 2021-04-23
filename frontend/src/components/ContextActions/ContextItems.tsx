// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import EditSvg from "../../assets/svg/edit.svg";
import "./ContextActions.scss";
import DeleteSvg from "../../assets/svg/delete.svg";
import MetadataSvg from "../../assets/svg/metadata.svg";
import RenameSvg from "../../assets/svg/rename.svg";
import DefaultSvg from "../../assets/svg/default.svg";

export type ContextItemType = "edit" | "delete" | "metadata" | "rename" | "primary";

interface ContextItemProps {
  type: ContextItemType;
  onClick: () => void;
}

export function ContextItem({ type, onClick }: ContextItemProps): React.ReactElement {
  return (
    <div
      key="edit"
      className="contextActions__item"
      onMouseDown={e => {
        onClick();
        e.stopPropagation();
      }}
      onClick={e => e.stopPropagation()}
    >
      {type === "edit" ? (
        <>
          <EditSvg /> Edit
        </>
      ) : type === "delete" ? (
        <>
          <DeleteSvg /> Delete
        </>
      ) : type === "metadata" ? (
        <>
          <MetadataSvg /> Metadata
        </>
      ) : type === "rename" ? (
        <>
          {" "}
          <RenameSvg /> Rename
        </>
      ) : type === "primary" ? (
        <>
          {" "}
          <DefaultSvg /> Make primary
        </>
      ) : (
        ""
      )}
    </div>
  );
}
