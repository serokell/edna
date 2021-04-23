// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { Suspense } from "react";
import "./Spinner.scss";
import cx from "classnames";

export function SuspenseSpinner({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}): React.ReactElement {
  return <Suspense fallback={<div className={cx("spinner", className)} />}>{children}</Suspense>;
}
