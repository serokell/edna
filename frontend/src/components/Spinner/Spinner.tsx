// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import "./Spinner.scss";
import React from "react";
import cx from "classnames";

export function Spinner({ className }: { className?: string }): React.ReactElement {
  return <div className={cx("spinner", className)} />;
}
