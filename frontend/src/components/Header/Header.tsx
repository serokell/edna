import React, { FunctionComponent, ReactElement } from "react";
import { Link, useLocation } from "react-router-dom";
import "./Header.scss";
import cx from "classnames";
import cn from "../../utils/bemUtils";
import { capitalizeFirstLetter } from "../../utils/utils";

const Header: FunctionComponent = (): ReactElement => {
  const ednaHeader = cn("ednaHeader");
  const loc = useLocation().pathname;
  const className = (sec: string) =>
    cx(ednaHeader("menuitem"), {
      ednaHeader__menuitem_active: loc.startsWith(sec),
    });

  const renderLink = (path: string) => (
    <Link key={path} to={`/${path}`} className={className(`/${path}`)}>
      {capitalizeFirstLetter(path)}
    </Link>
  );
  const links = ["dashboard", "upload", "library"];

  return (
    <div className="ednaHeader">
      <div className="ednaHeader__container">
        <span className={ednaHeader("brand")}>Edna</span>
        <div className={ednaHeader("menu")}>{links.map(renderLink)}</div>
      </div>
    </div>
  );
};

export default Header;
