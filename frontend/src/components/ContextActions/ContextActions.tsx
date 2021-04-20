import React, { useRef, useState } from "react";
import cx from "classnames";
import DotsSvg from "../../assets/svg/dots.svg";
import "./ContextActions.scss";
import cn from "../../utils/bemUtils";
import { useClickOutsideCallback } from "../../utils/utils";

interface ContextActionsProps {
  actions: React.ReactNode[];
  className?: string;
}

export function ContextActions({ actions, className }: ContextActionsProps): React.ReactElement {
  const [visible, setVisible] = useState(false);
  const contentActions = cn("contextActions");
  const ref = useRef(null);
  useClickOutsideCallback(ref, () => setVisible(false));

  return (
    <div ref={ref} className={cx("contextActions", className)}>
      <button
        type="button"
        className={contentActions("btn")}
        onClick={() => {
          setVisible(!visible);
        }}
      >
        <DotsSvg />
      </button>
      <div
        className={contentActions("content", { visible })}
        onClick={() => {
          setVisible(false);
        }}
      >
        {actions}
      </div>
    </div>
  );
}
