import React, { FunctionComponent } from "react";
import { useRecoilValue } from "recoil";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CompoundSelector, ProjectSelector, TargetSelector } from "./EntitySelector";
import "./DashboardPage.scss";
import { ExperimentsTableSuspendable } from "./ExperimentsTable";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import cn from "../../utils/bemUtils";
import { experimentsTableSizeAtom } from "../../store/atoms";
import PlotlyChart from "./Plotting";
import { selectedSubExperimentsQuery } from "../../store/selectors";
import { negateTableSize } from "../../store/types";

export const DashboardPage: FunctionComponent = () => {
  const dashboardPage = cn("dashboardPage");
  const expTableSize = useRecoilValue(experimentsTableSizeAtom);

  return (
    <PageLayout>
      <div className="dashboardPage">
        <ProjectSelector className={dashboardPage("projectSelector")} />
        <CompoundSelector className={dashboardPage("compoundSelector")} />
        <TargetSelector className={dashboardPage("targetSelector")} />
        <SuspenseSpinner className={dashboardPage("spinner")}>
          <>
            <PlotlyChartSuspendable
              className={dashboardPage("chart", { size: negateTableSize(expTableSize) })}
            />
            <ExperimentsTableSuspendable
              className={dashboardPage("experiments", { size: expTableSize })}
            />
          </>
        </SuspenseSpinner>
      </div>
    </PageLayout>
  );
};

interface PlotlyChartSuspendableProps {
  className?: string;
}

export function PlotlyChartSuspendable({
  className,
}: PlotlyChartSuspendableProps): React.ReactElement {
  const subExperiments = useRecoilValue(selectedSubExperimentsQuery);
  return <PlotlyChart className={className} subExperiments={subExperiments} />;
}
