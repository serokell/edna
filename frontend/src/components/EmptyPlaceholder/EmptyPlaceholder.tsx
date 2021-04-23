// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { ReactElement, ReactNode } from "react";
import "./EmptyPlaceholder.scss";
import cx from "classnames";

interface EmptyPlaceholderProps {
  title: string;
  description?: string;
  button?: ReactNode;
  className?: string;
}

export function EmptyPlaceholder({
  title,
  description,
  button,
  className,
}: EmptyPlaceholderProps): ReactElement {
  return (
    <div className={cx("emptyPlaceholder", className)}>
      <div className="emptyPlaceholder__title">{title}</div>
      {description && <div className="emptyPlaceholder__description">{description}</div>}
      {button && <div className="emptyPlaceholder__button">{button}</div>}
    </div>
  );
}
