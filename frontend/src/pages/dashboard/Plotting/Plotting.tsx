import * as PlotlyBasic from "plotly.js-basic-dist";
import createPlotlyComponent from "react-plotly.js/factory";
import React from "react";
import { v4 as uuidv4 } from "uuid";
import cx from "classnames";
import "./Plotting.scss";
import { useRecoilState } from "recoil";
import { EmptyPlaceholder } from "../../../components/EmptyPlaceholder/EmptyPlaceholder";
import { SubExperimentWithMeasurements } from "../../../store/types";
import { disabledPointsAtom } from "../../../store/atoms";
import { isDefined, Maybe } from "../../../utils/utils";

// import Plot from "react-plotly.js";

const plotConfig = {
  displaylogo: false,
  // displayModeBar: false,
};

interface PlotlyChartProps {
  className?: string;
  subExperiments: [SubExperimentWithMeasurements, string][];
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
  const [disabledPoints, setDisabledPoints] = useRecoilState(disabledPointsAtom);

  const fourPLlines: Plotly.Data[] = subExperiments.map(([sex, color]) => {
    return {
      name: "4PL",
      x: sex.measurements.map(a => a.item.concentration),
      y: sex.measurements.map(a => fourPL(sex.meta.item.result, a.item.concentration)),
      type: "scatter",
      mode: "lines",
      marker: { color },
    };
  });

  const plotlyData: Plotly.Data[] = subExperiments.map(([sex, color]) => {
    return {
      name: `${sex.meta.item.name}`,
      x: sex.measurements
        // .filter(x => !isDefined(disabledPoints.points.find(z => z.id === x.id)))
        .map(x => x.item.concentration),
      y: sex.measurements
        // .filter(y => !isDefined(disabledPoints.points.find(z => z.id === y.id)))
        .map(y => y.item.signal),
      type: "scatter",
      mode: "markers",
      marker: { color, size: 8 },
    };
  });

  function buildDisabledPlotlyData(): Maybe<Plotly.Data> {
    if (disabledPoints.curveNumber === -1) {
      return undefined;
    }
    const color = subExperiments[disabledPoints.curveNumber][1];
    return {
      name: subExperiments[disabledPoints.curveNumber][0].meta.item.name,
      x: disabledPoints.points.map(x => x.item.concentration),
      y: disabledPoints.points.map(y => y.item.signal),
      type: "scatter",
      mode: "markers",
      marker: {
        color,
        size: 8,
        line: {
          color: "#3a8370",
          width: 2,
        },
      },
    };
  }
  const disabledPlotlyData = buildDisabledPlotlyData();

  if (subExperiments.length === 0) {
    return (
      <EmptyPlaceholder
        className={className}
        title="Select sub-experiments to show them on chart"
      />
    );
  }
  const disabledCurveNumber = 2 * subExperiments.length;

  // TODO add disabled points from subplots
  const resultedPlotlyData = plotlyData.concat(fourPLlines);
  if (isDefined(disabledPlotlyData)) {
    resultedPlotlyData.push(disabledPlotlyData);
  }

  return (
    <Plot
      key={chartKey}
      className={cx("compoundPlot", className)}
      data={resultedPlotlyData}
      onClick={e => {
        const point = e.points[0];

        if (point.curveNumber === disabledCurveNumber) {
          // If clicked point is on disabled curve
          const reEnabledPoint = disabledPoints.points[point.pointIndex];
          const removed = disabledPoints.points.filter(x => x.id !== reEnabledPoint.id);
          if (removed.length === 0) {
            setDisabledPoints({
              curveNumber: -1,
              points: [],
            });
          } else {
            setDisabledPoints(old => ({
              curveNumber: old.curveNumber,
              points: removed,
            }));
          }
        }
        if (point.curveNumber >= subExperiments.length) {
          return;
        }

        const measPoint = subExperiments[point.curveNumber][0].measurements[point.pointIndex];
        if (disabledPoints.curveNumber === -1) {
          // If disabled set is empty, just add new point
          setDisabledPoints({
            curveNumber: point.curveNumber,
            points: [measPoint],
          });
        } else if (disabledPoints.curveNumber === point.curveNumber) {
          // If existing points belong to the same curveNumber as clicked point
          setDisabledPoints({
            curveNumber: point.curveNumber,
            points: disabledPoints.points.concat(measPoint),
          });
        }
      }}
      onRelayout={newLayout => {
        console.log("new layout", newLayout);
      }}
      layout={{
        xaxis: {
          type: "log",
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
        hovermode: "closest",
      }}
      config={plotConfig}
    />
  );
}
