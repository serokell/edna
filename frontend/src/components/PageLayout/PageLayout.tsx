import React from "react";
import Header from "../Header/Header";
import "./PageLayout.scss";

interface PageLayoutProps {
  children: React.ReactNode;
}

export default function PageLayout({ children }: PageLayoutProps): React.ReactElement {
  return (
    <>
      <Header />
      <div className="container">{children}</div>
    </>
  );
}
