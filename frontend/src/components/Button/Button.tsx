import React from "react";
import "../RoundSpinner.scss";
import "./Button.scss";
import cx from "classnames";
import cn from "../../utils/bemUtils";

type ButtonType = "primary" | "text" | "rounded" | "half-rounded" | "link";
type ButtonSize = "default" | "small";
type ButtonStyle = "delete" | "gray";

interface ButtonProps {
  type: ButtonType;
  size?: ButtonSize;
  className?: string;
  disabled?: boolean;
  isSubmit?: boolean;
  btnStyle?: ButtonStyle;
  children: React.ReactNode;
  loading?: boolean;
  active?: boolean;
  onClick?: () => void;

  [prop: string]: any;
}

// TODO add sizes
export function Button({
  type,
  size,
  className,
  btnStyle,
  disabled,
  children,
  loading,
  isSubmit,
  active,
  onClick,
  ...props
}: ButtonProps): React.ReactElement {
  const btnSize = size ?? "default";

  const baseClassNm = type === "half-rounded" ? "roundedButton" : `${type}Button`;

  return (
    <button
      onClick={e => {
        if (onClick) {
          onClick();
        }
        e.stopPropagation();
      }}
      {...props}
      disabled={disabled}
      type={isSubmit ? "submit" : "button"}
      className={cx(
        [className],
        cn(baseClassNm)({
          active,
          gray: btnStyle === "gray",
          delete: btnStyle === "delete",
          small: btnSize === "small",
          halfRounding: type === "half-rounded",
        })
      )}
    >
      {children} {loading && <div className="round-spinner btn-spinner" />}
    </button>
  );
}
