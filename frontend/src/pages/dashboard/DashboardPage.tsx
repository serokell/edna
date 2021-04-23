// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent } from "react";
import { useRecoilValue, waitForAll } from "recoil";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CompoundSelector, ProjectSelector, TargetSelector } from "./EntitySelector";
import "./DashboardPage.scss";
import { ExperimentsTableSuspendable } from "./ExperimentsTable/ExperimentsTable";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import cn from "../../utils/bemUtils";
import {
  experimentsTableSizeAtom,
  newSubexperimentAtom,
  selectedSubExperimentsColorAtom,
} from "../../store/atoms";
import PlotlyChart from "./Plotting/Plotting";
import { selectedSubExperimentsQuery } from "../../store/selectors";
import { negateTableSize, SuccessSubExperimentWithMeasurements } from "../../store/types";
import { isDefined, zip } from "../../utils/utils";
import { NewSubexperimentPlate } from "./NewSubexperimentPlate/NewSubexperimentPlate";

export const DashboardPage: FunctionComponent = () => {
  const dashboardPage = cn("dashboardPage");
  const expTableSize = useRecoilValue(experimentsTableSizeAtom);
  const plotlyClassName = dashboardPage("chart", { size: negateTableSize(expTableSize) });
  const experimentsClassName = dashboardPage("experiments", { size: expTableSize });
  const newSubexperiment = useRecoilValue(newSubexperimentAtom);

  return (
    <PageLayout>
      <div className="dashboardPage">
        <ProjectSelector className={dashboardPage("projectSelector")} />
        <CompoundSelector className={dashboardPage("compoundSelector")} />
        <TargetSelector className={dashboardPage("targetSelector")} />

        <SuspenseSpinner className={plotlyClassName}>
          <div className={plotlyClassName}>
            <PlotlyChartSuspendable />
            {newSubexperiment.subExperimentId !== -1 && (
              <NewSubexperimentPlate className="dashboardPage__newSubexperimentPlate" />
            )}
          </div>
        </SuspenseSpinner>

        <SuspenseSpinner className={experimentsClassName}>
          <ExperimentsTableSuspendable className={experimentsClassName} />
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
  const subExperiments = useRecoilValue(selectedSubExperimentsQuery).map(sex => ({
    meta: sex.meta,
    measurements: sex.measurements
      .slice()
      .sort((x, y) => x.item.concentration - y.item.concentration),
  }));
  const colors = useRecoilValue(
    waitForAll(subExperiments.map(sub => selectedSubExperimentsColorAtom(sub.meta.id)))
  );

  return (
    <PlotlyChart
      className={className}
      subExperiments={zip(subExperiments, colors)
        .filter(([sub, color]) => isDefined(color) && "Right" in sub.meta.item.result)
        .map(([subexperiment, color]) => ({
          subexperiment: subexperiment as SuccessSubExperimentWithMeasurements,
          color: color!,
        }))}
    />
  );
}
