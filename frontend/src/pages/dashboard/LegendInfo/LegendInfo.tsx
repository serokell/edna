// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement, useState } from "react";
import cn from "../../../utils/bemUtils";
import "./LegendInfo.scss";
import InfoSvg from "../../../assets/svg/info.svg";
import LegendSvg from "../../../assets/svg/legend.svg";
import { SubExperimentNColor } from "../Plotting/Plotting";

export interface LegendInfoProps {
  classname: string;
  subExperiments: SubExperimentNColor[];
}

export const LegendInfo: FunctionComponent<LegendInfoProps> = ({
  classname,
  subExperiments,
}): ReactElement => {
  const legendInfo = cn("legendInfo");
  const [legendActive, setLegendActive] = useState(false);

  return (
    <div className={legendInfo(null, [classname])}>
      <div
        className={legendInfo("icon")}
        onMouseEnter={() => setLegendActive(true)}
        onMouseLeave={() => setLegendActive(false)}
      >
        <InfoSvg />
      </div>
      <div className={legendInfo("legend", { invisible: !legendActive })}>
        {subExperiments.map(sex => (
          <div key={sex.subexperiment.meta.id} className={legendInfo("legendElement")}>
            <div>
              <LegendSvg fill={sex.color} stroke={sex.color} />
            </div>
            <span>{`(${sex.subexperiment.target} ⟶ ${sex.subexperiment.compound})`}</span>
            <div>
              <span className={legendInfo("ic50")}>IC50:</span>
              <span>{` ${sex.subexperiment.meta.item.result.Right[2]}`}</span>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};
