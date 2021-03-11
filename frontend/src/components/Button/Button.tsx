import React from "react";
import cn from "../../utils/bemUtils";
import "./Button.scss";

type ButtonType = "submit" | "primary" | "outlined" | "secondary" | "link";

interface ButtonProps {
  type: ButtonType;
  className?: string;
  disabled?: boolean;
  children: React.ReactNode;

  [prop: string]: any;
}

// TODO add sizes
export function Button({ type, className, disabled, children, ...props }: ButtonProps) {
  const btnClassName = cn(
    type === "submit" || type === "primary"
      ? "primaryButton"
      : type === "outlined"
      ? "outlinedButton"
      : type === "secondary"
      ? "secondaryButton"
      : type === "link"
      ? "linkButton"
      : ""
  )({
    disabled,
  });

  return (
    <button
      {...props}
      disabled={disabled}
      type={type === "submit" ? "submit" : "button"}
      className={`${btnClassName} ${className ?? ""}`}
    >
      {children}
    </button>
  );
}
