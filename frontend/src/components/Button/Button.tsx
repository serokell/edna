import React from "react";
import "../RoundSpinner.scss";
import "./Button.scss";
import cx from "classnames";

type ButtonType = "submit" | "primary" | "outlined" | "rounded" | "small-rounded" | "link";
type ButtonSize = "default" | "small";

interface ButtonProps {
  type: ButtonType;
  size?: ButtonSize;
  className?: string;
  disabled?: boolean;
  children: React.ReactNode;
  loading?: boolean;

  [prop: string]: any;
}

// TODO add sizes
export function Button({
  type,
  size,
  className,
  disabled,
  children,
  loading,
  ...props
}: ButtonProps): React.ReactElement {
  const btnSize = size ?? "default";

  const buttonModifiers = {
    primaryButton: type === "submit" || type === "primary",
    outlinedButton: type === "outlined",
    roundedButton: type === "rounded" || type === "small-rounded",
    linkButton: type === "link",
  };

  return (
    <button
      {...props}
      disabled={disabled}
      type={type === "submit" ? "submit" : "button"}
      className={cx(buttonModifiers, [className], {
        roundedButton_smallRounding: type === "small-rounded",
        "roundedButton_size-small": btnSize === "small",
      })}
    >
      {children} {loading && <div className="round-spinner btn-spinner" />}
    </button>
  );
}
