// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { useRecoilState } from "recoil";
import { ExperimentsTableSize } from "../../../store/types";
import MinimizeSvg from "../../../assets/svg/minimize.svg";
import ExpandSvg from "../../../assets/svg/expand.svg";
import "./ExpandMinimizeButton.scss";
import { experimentsTableSizeAtom } from "../../../store/atoms";

interface ExpandMinimizeButtonProps {
  targetSize: ExperimentsTableSize;
}

export function ExpandMinimizeButton({
  targetSize,
}: ExpandMinimizeButtonProps): React.ReactElement {
  const [expTableSize, setExpTableSize] = useRecoilState(experimentsTableSizeAtom);

  return (
    <div
      className="expandMinimizeBtn"
      onClick={() => {
        if (expTableSize !== targetSize) setExpTableSize("expanded");
        else setExpTableSize("minimized");
      }}
    >
      <span className="expandMinimizeBtn__icon">
        {expTableSize === targetSize ? <MinimizeSvg /> : <ExpandSvg />}
      </span>
      <span className="expandMinimizeBtn__label">
        {expTableSize === targetSize ? "Minimize" : "Expand"}
      </span>
    </div>
  );
}
