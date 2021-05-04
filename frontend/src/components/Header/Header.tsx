// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement } from "react";
import { Link, useLocation } from "react-router-dom";
import "./Header.scss";
import cn from "../../utils/bemUtils";
import LogoSvg from "../../assets/svg/logo.svg";

const Header: FunctionComponent = (): ReactElement => {
  const ednaHeader = cn("ednaHeader");
  const loc = useLocation().pathname;
  const className = (sec: string) => ednaHeader("menuitem", { active: loc.startsWith(sec) });

  const renderLink = (path: string) => (
    <Link key={path} to={`/${path}`} className={className(`/${path}`)}>
      {path}
    </Link>
  );
  const links = ["dashboard", "upload", "library"];

  return (
    <div className="ednaHeader">
      <div className="ednaHeader__container">
        <LogoSvg />
        <div className={ednaHeader("menu")}>{links.map(renderLink)}</div>
      </div>
    </div>
  );
};

export default Header;
