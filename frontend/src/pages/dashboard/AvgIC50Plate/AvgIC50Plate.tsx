// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { useRecoilValueLoadable } from "recoil";
import { selectedSubExperimentsIC50Query } from "../../../store/selectors";
import "./AvgIC50Plate.scss";
import { IC50Line } from "../../../components/IC50Line/IC50Line";

interface AvgIC50PlateProps {
  className?: string;
}

export function AvgIC50Plate({ className }: AvgIC50PlateProps): React.ReactElement {
  const ic50 = useRecoilValueLoadable(selectedSubExperimentsIC50Query);
  return (
    <div className={`avg50 ${className ?? ""}`}>
      <IC50Line label="avg IC50" ic50={ic50} />
    </div>
  );
}
