// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import * as PlotlyBasic from "plotly.js-basic-dist";
import createPlotlyComponent from "react-plotly.js/factory";
import React, { useState } from "react";
import { v4 as uuidv4 } from "uuid";
import cx from "classnames";
import "./Plotting.scss";
import { useRecoilState } from "recoil";
import { PlotMarker } from "plotly.js-basic-dist";
import { EmptyPlaceholder } from "../../../components/EmptyPlaceholder/EmptyPlaceholder";
import { SuccessSubExperimentWithMeasurements } from "../../../store/types";
import { newSubexperimentAtom } from "../../../store/atoms";
import { isDefined, logspace } from "../../../utils/utils";
import { useNotificationListUpdater } from "../../../store/updaters";

const plotConfig: Partial<PlotlyBasic.Config> = {
  displaylogo: false,
  modeBarButtonsToRemove: [
    "select2d",
    "lasso2d",
    "resetScale2d",
    "hoverClosestCartesian",
    "hoverCompareCartesian",
  ],
  // displayModeBar: false,
};

export interface SubExperimentNColor {
  subexperiment: SuccessSubExperimentWithMeasurements;
  color: string;
}

interface PlotlyChartProps {
  className?: string;
  subExperiments: SubExperimentNColor[];
}

function fourPL(result: number[], x: number) {
  const a = result[0];
  const b = result[1];
  const c = result[2];
  const d = result[3];
  return d + (a - d) / (1 + (x / c) ** b);
}

export default function PlotlyChart({
  subExperiments,
  className,
}: PlotlyChartProps): React.ReactElement {
  const chartKey = uuidv4();
  const Plot = createPlotlyComponent(PlotlyBasic);
  const [plotLayout, setPlotLayout] = useState<Partial<PlotlyBasic.Layout>>({
    xaxis: {
      type: "log" as const,
      autorange: true,
      title: "Concentration [uM]",
    },
    yaxis: {
      autorange: true,
      title: "Signal",
    },
    showlegend: false,
    height: 534,
    margin: {
      t: 0,
      r: 0,
    },
    hovermode: "closest" as const,
    hoverlabel: { bgcolor: "rgba(25, 28, 28, 0.7)" },
    paper_bgcolor: "rgba(0,0,0,0)",
    plot_bgcolor: "rgba(0,0,0,0)",
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    modebar: {
      bgcolor: "rgba(0,0,0,0)",
      color: "rgba(25, 28, 28, 0.3)",
      activecolor: "rgba(25, 28, 28, 0.7)",
    },
  });
  const [newSubexperiment, setNewSubexperiment] = useRecoilState(newSubexperimentAtom);
  const notificationsUpdater = useNotificationListUpdater();

  if (subExperiments.length === 0) {
    return (
      <EmptyPlaceholder
        className={className}
        title="Select sub-experiments to show them on chart"
      />
    );
  }

  const enabledTraces: Plotly.Data[] = subExperiments.map(({ subexperiment, color }) => {
    const pointsToDraw = subexperiment.measurements.filter(
      x => !isDefined(newSubexperiment.changedPoints.find(z => z.id === x.id)) && x.item.isEnabled
    );

    return {
      text: Array(pointsToDraw.length).fill(`${subexperiment.meta.item.name}`),
      x: pointsToDraw.map(x => x.item.concentration),
      y: pointsToDraw.map(y => y.item.signal),
      type: "scatter",
      mode: "markers",
      marker: { color, size: 8 },
      hovertemplate: "%{text}<br>Concentration: %{x}<br>Signal: %{y}<extra></extra>",
    };
  });

  const disabledTraces: PlotlyBasic.Data[] = subExperiments.map(({ subexperiment }) => {
    const pointsToDraw = subexperiment.measurements.filter(
      x => !isDefined(newSubexperiment.changedPoints.find(z => z.id === x.id)) && !x.item.isEnabled
    );

    return {
      x: pointsToDraw.map(x => x.item.concentration),
      y: pointsToDraw.map(y => y.item.signal),
      type: "scatter",
      mode: "markers",
      marker: { color: "#AFB6B6", size: 8 },
      hovertemplate: `%{${subexperiment.meta.item.name}} (disabled)<br>Concentration: %{x}<br>Signal: %{y}<extra></extra>`,
    };
  });

  const plTraces: Plotly.Data[] = subExperiments.map(({ subexperiment, color }) => {
    const x = logspace(
      subexperiment.measurements[0].item.concentration,
      subexperiment.measurements[subexperiment.measurements.length - 1].item.concentration,
      1000
    );
    return {
      x,
      y: x.map(a => fourPL(subexperiment.meta.item.result.Right, a)),
      type: "scatter",
      mode: "lines",
      marker: { color },
      hovertemplate: `(${subexperiment.compound} ⟶ ${subexperiment.target})<br>IC50: ${subexperiment.meta.item.result.Right[2]}<extra></extra>`,
    };
  });

  function buildNewSubexperimentTrace(): PlotlyBasic.Data[] {
    if (newSubexperiment.subExperimentId === -1) {
      return [];
    }
    const sub = subExperiments.find(
      s => s.subexperiment.meta.id === newSubexperiment.subExperimentId
    );
    if (!isDefined(sub)) {
      return [];
    }

    const buildTrace = (isEnabled: boolean, marker: Partial<PlotMarker>): PlotlyBasic.Data => {
      const neededPoints = newSubexperiment.changedPoints.filter(
        x => x.item.isEnabled === isEnabled
      );
      return {
        x: neededPoints.map(x => x.item.concentration),
        y: neededPoints.map(y => y.item.signal),
        type: "scatter",
        mode: "markers",
        marker,
        hovertemplate: `%{${sub.subexperiment.meta.item.name}}<br>Concentration: %{x}<br>Signal: %{y}<extra></extra>`,
      };
    };

    return [
      // Disabled trace
      buildTrace(true, {
        color: sub.color,
        size: 10,
        line: {
          color: "#ce6c6c",
          width: 3,
        },
      }),

      buildTrace(false, {
        color: "#AFB6B6",
        size: 10,
        line: {
          color: "#3a8370",
          width: 3,
        },
      }),
    ];
  }
  const newSubexperimentTrace = buildNewSubexperimentTrace();
  const newSubCurveNumber = 3 * subExperiments.length;

  const resultedPlotlyData = enabledTraces
    .concat(disabledTraces)
    .concat(plTraces)
    .concat(newSubexperimentTrace);

  return (
    <Plot
      key={chartKey}
      className={cx("compoundPlot", className)}
      data={resultedPlotlyData}
      onClick={e => {
        const point = e.points[0];

        if (
          point.curveNumber === newSubCurveNumber ||
          point.curveNumber === newSubCurveNumber + 1
        ) {
          // Reset current IC50
          setNewSubexperiment(old => ({
            ...old,
            analysed: undefined,
          }));
          // If clicked point is initially enabled
          const isEnabled = point.curveNumber === newSubCurveNumber;
          const revertedPoint = newSubexperiment.changedPoints.filter(
            x => x.item.isEnabled === isEnabled
          )[point.pointIndex];
          const newChanged = newSubexperiment.changedPoints.filter(x => x.id !== revertedPoint.id);

          setNewSubexperiment(old => ({
            subExperimentId: newChanged.length === 0 ? -1 : old.subExperimentId,
            changedPoints: newChanged,
          }));
        }

        if (point.curveNumber >= 2 * subExperiments.length) {
          return;
        }

        const enabledTr = point.curveNumber < subExperiments.length;
        const subExpOfClickedTrace =
          subExperiments[point.curveNumber % subExperiments.length].subexperiment;
        // Take rest of points on the corresponding line (enabled or disabled),
        // which are not put in the changedSet yet.
        const measPoint = subExpOfClickedTrace.measurements.filter(
          p =>
            p.item.isEnabled === enabledTr &&
            !isDefined(newSubexperiment.changedPoints.find(pp => pp.id === p.id))
        )[point.pointIndex];

        if (
          newSubexperiment.subExperimentId === -1 ||
          newSubexperiment.subExperimentId === subExpOfClickedTrace.meta.id
        ) {
          // Reset current IC50
          setNewSubexperiment(old => ({
            ...old,
            analysed: undefined,
          }));

          // If disabled set is empty, just add new point
          setNewSubexperiment(old => ({
            subExperimentId: subExpOfClickedTrace.meta.id,
            changedPoints: old.changedPoints.concat(measPoint),
          }));
        } else {
          notificationsUpdater({
            type: "Add",
            notificationType: "Warn",
            element: () => (
              <span>You are only allowed to enable/disable points on one sub-experiment</span>
            ),
          });
        }
      }}
      onRelayout={newLayout => {
        setPlotLayout(old => ({
          ...old,
          newLayout,
        }));
      }}
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      layout={plotLayout}
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      config={plotConfig}
    />
  );
}
