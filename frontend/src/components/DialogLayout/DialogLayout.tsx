import React, { FunctionComponent } from "react";
import CrossSvg from "../../assets/svg/cross.svg";
import "./DialogLayout.scss";

interface DialogLayoutProps {
  title: string;
  description?: string;
  children: React.ReactElement;
  onClose: () => void;
}

export const DialogLayout: FunctionComponent<DialogLayoutProps> = ({
  title,
  description,
  children,
  onClose,
}) => {
  return (
    <div className="dialogWindow__background" onClick={onClose}>
      <div
        className="dialogWindow"
        onClick={e => {
          e.stopPropagation();
        }}
      >
        <div className="dialogWindow__close" onClick={onClose}>
          <CrossSvg />
        </div>
        <div className="dialogWindow__title">{title}</div>
        {description && <div className="dialogWindow__description">{description} </div>}
        <div className="dialogWindow__spacer" />
        {children}
      </div>
    </div>
  );
};
