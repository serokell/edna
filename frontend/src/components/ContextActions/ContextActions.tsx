import React from "react";
import DotsSvg from "../../assets/svg/dots.svg";
import "./ContextActions.scss";

interface ContextActionsProps {
  actions: React.ReactNode[];
}

export function ContextActions({ actions }: ContextActionsProps): React.ReactElement {
  return (
    <div className="contextActions__wrapper">
      <button type="button" className="contextActions__btn">
        <DotsSvg />
      </button>
      <div className="contextActions__content">{actions}</div>
    </div>
  );
}
