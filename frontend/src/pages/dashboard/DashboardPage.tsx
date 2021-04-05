import React, { FunctionComponent } from "react";
import { useRecoilValue } from "recoil";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CompoundSelector, ProjectSelector, TargetSelector } from "./EntitySelector";
import "./DashboardPage.scss";
import { ExperimentsTableSuspendable } from "./ExperimentsTable";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import cn from "../../utils/bemUtils";
import { experimentsTableSizeAtom } from "../../store/atoms";

export const DashboardPage: FunctionComponent = () => {
  const dashboardPage = cn("dashboardPage");
  const expTableSize = useRecoilValue(experimentsTableSizeAtom);

  return (
    <PageLayout>
      <div className="dashboardPage">
        <ProjectSelector className={dashboardPage("projectSelector")} />
        <CompoundSelector className={dashboardPage("compoundSelector")} />
        <TargetSelector className={dashboardPage("targetSelector")} />
        <SuspenseSpinner>
          <ExperimentsTableSuspendable
            className={dashboardPage("experiments", { size: expTableSize })}
          />
        </SuspenseSpinner>
      </div>
    </PageLayout>
  );
};
