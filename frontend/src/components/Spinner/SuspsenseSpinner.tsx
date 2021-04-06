import React, { Suspense } from "react";
import "./Spinner.scss";
import cx from "classnames";

export function SuspenseSpinner({
  children,
  className,
}: {
  children: React.ReactElement;
  className?: string;
}): React.ReactElement {
  return <Suspense fallback={<div className={cx("spinner", className)} />}>{children}</Suspense>;
}
