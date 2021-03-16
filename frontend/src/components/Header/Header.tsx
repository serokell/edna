import React, { FunctionComponent, ReactElement } from "react";
import { Link, useLocation } from "react-router-dom";
import "./Header.scss";
import cn from "../../utils/bemUtils";
import ExcludeSvg from "../../assets/svg/exclude.svg";

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
        <span className={ednaHeader("brand")}>
          <ExcludeSvg className="ednaHeader__ednaLogo" />
          edna
        </span>
        <div className={ednaHeader("menu")}>{links.map(renderLink)}</div>
      </div>
    </div>
  );
};

export default Header;
