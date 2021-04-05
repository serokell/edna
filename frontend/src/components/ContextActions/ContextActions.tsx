import React from "react";
import cx from "classnames";
import DotsSvg from "../../assets/svg/dots.svg";
import "./ContextActions.scss";

interface ContextActionsProps {
  actions: React.ReactNode[];
  className?: string;
}

export function ContextActions({ actions, className }: ContextActionsProps): React.ReactElement {
  return (
    <div className={cx("contextActions__wrapper", className)}>
      <button type="button" className="contextActions__btn">
        <DotsSvg />
      </button>
      <div className="contextActions__content">{actions}</div>
    </div>
  );
}
