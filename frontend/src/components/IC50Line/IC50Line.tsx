// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import "./IC50Line.scss";
import { IC50Tooltip, IC50Value } from "./IC50Tooltip";

interface IC50LineProps {
  label: string;
  ic50: IC50Value;
}

export function IC50Line({ label, ic50 }: IC50LineProps): React.ReactElement {
  return (
    <div className="ic50">
      <span className="ic50__label">{label}</span>
      <IC50Tooltip ic50={ic50} />
    </div>
  );
}
