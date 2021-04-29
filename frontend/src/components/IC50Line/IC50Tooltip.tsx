// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Loadable } from "recoil";
import React from "react";
import { ResultDto } from "../../api/types";
import { Tooltip } from "../Tooltip/Tooltip";
import { formatIC50 } from "../../utils/utils";

export type IC50Value = ResultDto<number | number[]> | Loadable<ResultDto<number | number[]>>;

interface IC50TooltipProps {
  ic50: IC50Value;
}

export function IC50Tooltip({ ic50 }: IC50TooltipProps): React.ReactElement {
  const errTooltip = (msg: string) => {
    return (
      <Tooltip text={msg} type="error">
        <span className="ic50__valueNone" />
      </Tooltip>
    );
  };

  const valueTooltip = (x: number | number[]) => {
    if (typeof x === "number") {
      return (
        <Tooltip text={`${x}`}>
          <span className="ic50__value">{formatIC50(x)}</span>
        </Tooltip>
      );
    }
    return (
      <Tooltip text={`${x[2]}`}>
        <span className="ic50__value">{formatIC50(x[2])}</span>
      </Tooltip>
    );
  };

  if ("state" in ic50) {
    if (ic50.state === "hasError") {
      return errTooltip(ic50.contents.message);
    }
    if (ic50.state === "hasValue" && "Left" in ic50.contents) {
      return errTooltip(ic50.contents.Left);
    }
    if (ic50.state === "loading") {
      return (
        <Tooltip text="Loading...">
          <span className="ic50__valueNone" />
        </Tooltip>
      );
    }
    if (ic50.state === "hasValue" && "Right" in ic50.contents) {
      return valueTooltip(ic50.contents.Right);
    }
  } else if ("Left" in ic50) {
    return errTooltip(ic50.Left);
  } else if ("Right" in ic50) {
    return valueTooltip(ic50.Right);
  }
  return <></>;
}
