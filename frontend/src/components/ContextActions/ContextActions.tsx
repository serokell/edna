import React, { useState } from "react";
import cx from "classnames";
import DotsSvg from "../../assets/svg/dots.svg";
import "./ContextActions.scss";
import cn from "../../utils/bemUtils";

interface ContextActionsProps {
  actions: React.ReactNode[];
  className?: string;
}

export function ContextActions({ actions, className }: ContextActionsProps): React.ReactElement {
  const [visible, setVisible] = useState(false);
  const contentActions = cn("contextActions");

  return (
    <div className={cx("contextActions", className)}>
      <button
        type="button"
        className={contentActions("btn")}
        onBlur={() => {
          setVisible(false);
        }}
        onClick={() => setVisible(!visible)}
      >
        <DotsSvg />
      </button>
      <div className={contentActions("content", { visible })}>{actions}</div>
    </div>
  );
}
