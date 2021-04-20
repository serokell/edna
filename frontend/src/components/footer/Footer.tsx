import React, { FunctionComponent, ReactElement } from "react";
import cn from "../../utils/bemUtils";
import "./Footer.scss";

const Footer: FunctionComponent = (): ReactElement => {
  const footer = cn("footer");

  return (
    <div className={footer()}>
      <div className={footer("container")}>
        <span className={footer("description")}>© Serokell 2021</span>
      </div>
    </div>
  );
};

export default Footer;
