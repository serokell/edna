// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, useCallback, useEffect, useState } from "react";
import { useRecoilValue, useRecoilValueLoadable, useSetRecoilState, waitForAll } from "recoil";
import { useLocation } from "react-router-dom";
import { Button } from "../../components/Button/Button";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CompoundSelector, ProjectSelector, TargetSelector } from "./EntitySelector";
import "./DashboardPage.scss";
import { ExperimentsTableSuspendable } from "./ExperimentsTable/ExperimentsTable";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import cn from "../../utils/bemUtils";
import {
  compoundIdSelectedAtom,
  experimentsTableSizeAtom,
  newSubexperimentAtom,
  projectSelectedIdAtom,
  selectedSubExperimentsColorAtom,
  selectedSubExperimentsIdsAtom,
  targetIdSelectedAtom,
} from "../../store/atoms";
import PlotlyChart from "./Plotting/Plotting";
import { filteredExperimentsQuery, selectedSubExperimentsExtraQuery } from "../../store/selectors";
import {
  Experiment,
  negateTableSize,
  SuccessSubExperimentWithMeasurements,
} from "../../store/types";
import { isDefined, zip } from "../../utils/utils";
import { NewSubexperimentPlate } from "./NewSubexperimentPlate/NewSubexperimentPlate";
import { AvgIC50Plate } from "./AvgIC50Plate/AvgIC50Plate";
import { LegendInfo } from "./LegendInfo/LegendInfo";

export const DashboardPage: FunctionComponent = () => {
  const dashboardPage = cn("dashboardPage");
  const expTableSize = useRecoilValue(experimentsTableSizeAtom);
  const plotlyClassName = dashboardPage("chart", { size: negateTableSize(expTableSize) });
  const plotlySpinnerClassName = dashboardPage("spinner", { size: negateTableSize(expTableSize) });
  const experimentsClassName = dashboardPage("experiments", { size: expTableSize });
  const newSubexperiment = useRecoilValue(newSubexperimentAtom);
  const setProjectSelectedIdAtom = useSetRecoilState(projectSelectedIdAtom);
  const setCompoundSelectedIdAtom = useSetRecoilState(compoundIdSelectedAtom);
  const setTargetSelectedIdAtom = useSetRecoilState(targetIdSelectedAtom);
  const experimentsL = useRecoilValueLoadable(filteredExperimentsQuery({}));
  const [experiments, setExperiments] = useState<Experiment[] | undefined>(undefined);

  const loc = useLocation();
  useEffect(() => {
    if (isDefined(loc.state)) {
      const st = loc.state as any;
      setProjectSelectedIdAtom(st.projectId);
      setCompoundSelectedIdAtom(st.compoundId);
      setTargetSelectedIdAtom(st.targetId);
    }
  }, [loc.state, setProjectSelectedIdAtom, setCompoundSelectedIdAtom, setTargetSelectedIdAtom]);

  useEffect(() => {
    if (!experiments && experimentsL.state === "hasValue" && experimentsL.contents) {
      setExperiments(experimentsL.contents.experiments);
    }
  }, [experiments, experimentsL]);

  return (
    <PageLayout>
      <div className="dashboardPage">
        <div className={dashboardPage("titleBlock")}>
          <span className={dashboardPage("title")}>Dashboard</span>
          <Button
            type="rounded"
            className="dashboardPage__resetBtn"
            onClick={() => {
              setProjectSelectedIdAtom(undefined);
              setCompoundSelectedIdAtom(undefined);
              setTargetSelectedIdAtom(undefined);
            }}
          >
            Reset
          </Button>
        </div>
        <ProjectSelector className={dashboardPage("projectSelector")} experiments={experiments} />
        <CompoundSelector className={dashboardPage("compoundSelector")} experiments={experiments} />
        <TargetSelector className={dashboardPage("targetSelector")} experiments={experiments} />

        <SuspenseSpinner className={plotlySpinnerClassName}>
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
  const subExperiments = useRecoilValue(selectedSubExperimentsExtraQuery).map(sex => ({
    ...sex,
    measurements: sex.measurements
      .slice()
      .sort((x, y) => x.item.concentration - y.item.concentration),
  }));
  const colors = useRecoilValue(
    waitForAll(subExperiments.map(sub => selectedSubExperimentsColorAtom(sub.meta.id)))
  );
  const isSubsEmpty = useRecoilValue(selectedSubExperimentsIdsAtom).size === 0;

  const filterSubs = useCallback(
    () =>
      zip(subExperiments, colors)
        .filter(([sub, color]) => isDefined(color) && "Right" in sub.meta.item.result)
        .map(([subexperiment, color]) => ({
          subexperiment: subexperiment as SuccessSubExperimentWithMeasurements,
          color: color!,
        })),
    [subExperiments, colors]
  );

  return (
    <>
      <PlotlyChart className={className} subExperiments={filterSubs()} />
      {!isSubsEmpty && (
        <div className="dashboardPage__plotInfo">
          <AvgIC50Plate />
          <LegendInfo classname="dashboardPage__legend" subExperiments={filterSubs()} />
        </div>
      )}
    </>
  );
}
