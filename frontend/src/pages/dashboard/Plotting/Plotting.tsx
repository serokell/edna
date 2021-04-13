import * as PlotlyBasic from "plotly.js-basic-dist";
import createPlotlyComponent from "react-plotly.js/factory";
import React, { useState } from "react";
import { v4 as uuidv4 } from "uuid";
import cx from "classnames";
import "./Plotting.scss";
import { useRecoilState } from "recoil";
import { PlotMarker } from "plotly.js-basic-dist";
import { EmptyPlaceholder } from "../../../components/EmptyPlaceholder/EmptyPlaceholder";
import { SubExperimentWithMeasurements } from "../../../store/types";
import { newSubexperimentAtom } from "../../../store/atoms";
import { isDefined } from "../../../utils/utils";

const plotConfig: Partial<PlotlyBasic.Config> = {
  displaylogo: false,
  modeBarButtonsToRemove: ["select2d", "lasso2d", "resetScale2d"],
  // displayModeBar: false,
};

interface SubExperimentNColor {
  subexperiment: SubExperimentWithMeasurements;
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
      title: "Signal",
    },
    showlegend: false,
    margin: {
      t: 0,
      r: 0,
    },
    hovermode: "closest" as const,
  });
  const [newSubexperiment, setNewSubexperiment] = useRecoilState(newSubexperimentAtom);

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
      name: `${subexperiment.meta.item.name}`,
      x: pointsToDraw.map(x => x.item.concentration),
      y: pointsToDraw.map(y => y.item.signal),
      type: "scatter",
      mode: "markers",
      marker: { color, size: 8 },
    };
  });

  const disabledTraces: PlotlyBasic.Data[] = subExperiments.map(({ subexperiment }) => {
    const pointsToDraw = subexperiment.measurements.filter(
      x => !isDefined(newSubexperiment.changedPoints.find(z => z.id === x.id)) && !x.item.isEnabled
    );

    return {
      name: `${subexperiment.meta.item.name} (Disabled)`,
      x: pointsToDraw.map(x => x.item.concentration),
      y: pointsToDraw.map(y => y.item.signal),
      type: "scatter",
      mode: "markers",
      marker: { color: "#AFB6B6", size: 8 },
    };
  });

  const plTraces: Plotly.Data[] = subExperiments.map(({ subexperiment, color }) => {
    return {
      name: "4PL",
      x: subexperiment.measurements.map(a => a.item.concentration),
      y: subexperiment.measurements.map(a =>
        fourPL(subexperiment.meta.item.result, a.item.concentration)
      ),
      type: "scatter",
      mode: "lines",
      marker: { color },
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
        name: sub.subexperiment.meta.item.name,
        x: neededPoints.map(x => x.item.concentration),
        y: neededPoints.map(y => y.item.signal),
        type: "scatter",
        mode: "markers",
        marker,
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
