import React from "react";
import "./Button.scss";
import cx from "classnames";

type ButtonType = "submit" | "primary" | "outlined" | "secondary" | "link";

interface ButtonProps {
  type: ButtonType;
  className?: string;
  disabled?: boolean;
  children: React.ReactNode;

  [prop: string]: any;
}

// TODO add sizes
export function Button({
  type,
  className,
  disabled,
  children,
  ...props
}: ButtonProps): React.ReactElement {
  const buttonModifiers = {
    primaryButton: type === "submit" || type === "primary",
    outlinedButton: type === "outlined",
    secondaryButton: type === "secondary",
    linkButton: type === "link",
  };

  return (
    <button
      {...props}
      disabled={disabled}
      type={type === "submit" ? "submit" : "button"}
      className={cx(buttonModifiers, [className])}
    >
      {children}
    </button>
  );
}
