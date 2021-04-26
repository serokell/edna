// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import "./Tooltip.scss";
import cn from "../../utils/bemUtils";

interface TooltipProps {
  text: string;
  type?: "default" | "error";
  children: React.ReactNode;
}
export function Tooltip({ text, children, type }: TooltipProps): React.ReactElement {
  const typeV = type === "default" ? undefined : type;

  const tooltipContainer = cn("tooltipContainer");
  return (
    <>
      <span className={tooltipContainer()}>
        {children}
        <span className={tooltipContainer("tooltip", { type: typeV })}>{text}</span>
      </span>
    </>
  );
}
