import React, { Suspense } from "react";
import "./Spinner.scss";

export function SuspenseSpinner({
  children,
}: {
  children: React.ReactElement;
}): React.ReactElement {
  return <Suspense fallback={<div className="spinner" />}>{children}</Suspense>;
}
