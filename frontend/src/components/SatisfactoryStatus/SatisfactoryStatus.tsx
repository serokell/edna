// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import cx from "classnames";
import React from "react";
import StatusSvg from "../../assets/svg/status.svg";
import "./SatisfactoryStatus.scss";

interface SatisfactoryStatusProps {
  isSuspicious: boolean;
  onClick: () => void;
  className?: string;
}

export function SatisfactoryStatus({
  isSuspicious,
  className,
  onClick,
}: SatisfactoryStatusProps): React.ReactElement {
  return (
    <span
      onClick={e => {
        onClick();
        e.stopPropagation();
      }}
    >
      <StatusSvg
        className={cx(className, "satisfactoryStatus", {
          satisfactoryStatus__ok: !isSuspicious,
          satisfactoryStatus__bad: isSuspicious,
        })}
      />
    </span>
  );
}
