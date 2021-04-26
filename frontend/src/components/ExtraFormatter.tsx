// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { Tooltip } from "./Tooltip/Tooltip";

interface ExtraFormatterProps {
  items: string[];
}

export function ExtraFormatter({ items }: ExtraFormatterProps): React.ReactElement {
  if (items.length <= 4) {
    return <>{items.join(", ")}</>;
  }
  return (
    <>
      {`${items.slice(0, 4).join(", ")} and `}
      <Tooltip text={items.slice(4).join(", ")}>{`${items.length - 4} more`}</Tooltip>
    </>
  );
}
