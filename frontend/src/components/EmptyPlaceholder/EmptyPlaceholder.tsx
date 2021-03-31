import React, { ReactElement, ReactNode } from "react";
import "./EmptyPlaceholder.scss";

interface EmptyPlaceholderProps {
  title: string;
  description?: string;
  button?: ReactNode;
}

export function EmptyPlaceholder({
  title,
  description,
  button,
}: EmptyPlaceholderProps): ReactElement {
  return (
    <div className="emptyPlaceholder">
      <div className="emptyPlaceholder__title">{title}</div>
      {description && <div className="emptyPlaceholder__description">{description}</div>}
      {button && <div className="emptyPlaceholder__button">{button}</div>}
    </div>
  );
}
