// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import Footer from "../footer/Footer";
import Header from "../Header/Header";
import "./PageLayout.scss";

interface PageLayoutProps {
  children: React.ReactNode;
}

export default function PageLayout({ children }: PageLayoutProps): React.ReactElement {
  return (
    <div className="pageLayout">
      <Header />
      <div className="container">{children}</div>
      <Footer />
    </div>
  );
}
