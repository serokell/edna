// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import DotsSvg from "../../assets/svg/dots.svg";
import "./MethodologyPlate.scss";

interface MethodologyPlateProps {
  title: string;
  description?: string;
}

export function MethodologyPlate({
  title,
  description,
}: MethodologyPlateProps): React.ReactElement {
  return (
    <div key={title} className="methodologyPlate">
      <div className="methodologyPlate__title">{title}</div>
      <div className="methodologyPlate__contextActions">
        <DotsSvg />
      </div>
      <div className="methodologyPlate__description">{description}</div>
    </div>
  );
}
