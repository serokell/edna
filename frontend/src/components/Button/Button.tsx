import React from "react";
import "../RoundSpinner.scss";
import "./Button.scss";
import cx from "classnames";

type ButtonType = "submit" | "primary" | "text" | "rounded" | "half-rounded" | "link";
type ButtonSize = "default" | "small";
type ButtonStyle = "delete" | "gray";

interface ButtonProps {
  type: ButtonType;
  size?: ButtonSize;
  className?: string;
  disabled?: boolean;
  btnStyle?: ButtonStyle;
  children: React.ReactNode;
  loading?: boolean;

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
  ...props
}: ButtonProps): React.ReactElement {
  const btnSize = size ?? "default";

  const buttonModifiers = {
    primaryButton: type === "submit" || type === "primary",
    textButton: type === "text",
    roundedButton: type === "rounded" || type === "half-rounded",
    linkButton: type === "link",
  };

  return (
    <button
      {...props}
      disabled={disabled}
      type={type === "submit" ? "submit" : "button"}
      className={cx(buttonModifiers, [className], {
        roundedButton_halfRounding: type === "half-rounded",
        "roundedButton_size-small": btnSize === "small",
        textButton_gray: btnStyle === "gray",
        textButton_delete: btnStyle === "delete",
      })}
    >
      {children} {loading && <div className="round-spinner btn-spinner" />}
    </button>
  );
}
