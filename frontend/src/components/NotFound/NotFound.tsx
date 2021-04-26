// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import PageLayout from "../PageLayout/PageLayout";
import "./NotFound.scss";

export function NotFound(): React.ReactElement {
  return (
    <PageLayout>
      <span className="notFoundCentered">Здесь ничего нет</span>
    </PageLayout>
  );
}
