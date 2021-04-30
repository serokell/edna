// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement } from "react";
import cn from "../../utils/bemUtils";
import "./Footer.scss";

const Footer: FunctionComponent = (): ReactElement => {
  const footer = cn("footer");

  return (
    <div className={footer()}>
      <div className={footer("container")}>
        <span className={footer("description")}>© Serokell 20222</span>
      </div>
    </div>
  );
};

export default Footer;
