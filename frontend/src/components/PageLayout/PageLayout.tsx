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
