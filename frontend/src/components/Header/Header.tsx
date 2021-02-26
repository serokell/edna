import React, {FunctionComponent, ReactElement} from "react";
import "./Header.scss";
import cn from "../../utils/bemUtils";

const Header: FunctionComponent = (): ReactElement => {
  const ednaHeader = cn("ednaHeader")

  return <div className="ednaHeader">
    <span className={ednaHeader("brand")}>Edna</span>

    <div className={ednaHeader("menu")}>
      <a className={ednaHeader("menuitem")}>
        Dashboard
      </a>

      <a className="ednaHeader__menuitem ednaHeader__menuitem_active">
        Upload
      </a>

      <a className={ednaHeader("menuitem")}>
        Library
      </a>
    </div>
  </div>
}

export default Header;
