import cx from "classnames";
import React from "react";
import StatusSvg from "../../assets/svg/status.svg";
import "./SatisfactoryStatus.scss";

interface SatisfactoryStatusProps {
  isSuspicious: boolean;
  className?: string;
}

export function SatisfactoryStatus({
  isSuspicious,
  className,
}: SatisfactoryStatusProps): React.ReactElement {
  return (
    <StatusSvg
      className={cx(className, "satisfactoryStatus", {
        satisfactoryStatus__ok: !isSuspicious,
        satisfactoryStatus__bad: isSuspicious,
      })}
    />
  );
}
