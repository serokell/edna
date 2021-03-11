import React, { FunctionComponent, ReactElement } from "react";
import "./Header.scss";
import cn from "../../utils/bemUtils";

const Header: FunctionComponent = (): ReactElement => {
  const ednaHeader = cn("ednaHeader");

  return (
    <div className="ednaHeader">
      <div className="ednaHeader__container">
        <span className={ednaHeader("brand")}>Edna</span>

        <div className={ednaHeader("menu")}>
          {/* eslint-disable-next-line jsx-a11y/anchor-is-valid */}
          <a className={ednaHeader("menuitem")}>Dashboard</a>

          {/* eslint-disable-next-line jsx-a11y/anchor-is-valid */}
          <a className="ednaHeader__menuitem ednaHeader__menuitem_active">Upload</a>

          {/* eslint-disable-next-line jsx-a11y/anchor-is-valid */}
          <a className={ednaHeader("menuitem")}>Library</a>
        </div>
      </div>
    </div>
  );
};

export default Header;
