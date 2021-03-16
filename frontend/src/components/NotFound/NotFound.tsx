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
