import React, { Suspense } from "react";
import "./Spinner.scss";

export function SuspenseSpinner({ children }: { children: React.ReactNode }) {
  return <Suspense fallback={<div className="spinner" />}>{children}</Suspense>;
}
