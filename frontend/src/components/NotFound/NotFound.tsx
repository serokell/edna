// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import PageLayout from "../PageLayout/PageLayout";
import "./NotFound.scss";

interface NotFoundProps {
  page?: boolean;
}

export function NotFound({ page }: NotFoundProps): React.ReactElement {
  if (page) {
    return (
      <PageLayout>
        <span className="notFoundCentered">Здесь ничего нет</span>
      </PageLayout>
    );
  }
  return <span className="notFoundCentered">Здесь ничего нет</span>;
}
