import React, { useState } from "react";
import DotsSvg from "../../assets/svg/dots.svg";
import "./ContextActions.scss";
import cn from "../../utils/bemUtils";

interface ContextActionsProps {
  actions: React.ReactNode[];
}

export function ContextActions({ actions }: ContextActionsProps): React.ReactElement {
  const [visible, setVisible] = useState(false);
  const contentActions = cn("contextActions");

  return (
    <div className="contextActions">
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
      <div id="contextActionsContent" className={contentActions("content", { visible })}>
        {actions}
      </div>
    </div>
  );
}
