import React, { FunctionComponent } from "react";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CompoundSelector, ProjectSelector, TargetSelector } from "./EntitySelector";
import "./DashboardPage.scss";

export const DashboardPage: FunctionComponent = () => {
  return (
    <PageLayout>
      <div className="dashboardPage">
        <ProjectSelector className="dashboardPage__projectSelector" />
        <CompoundSelector className="dashboardPage__compoundSelector" />
        <TargetSelector className="dashboardPage__targetSelector" />
      </div>
    </PageLayout>
  );
};
