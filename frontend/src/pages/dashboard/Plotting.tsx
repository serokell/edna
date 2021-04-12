import * as PlotlyBasic from "plotly.js-basic-dist";
import createPlotlyComponent from "react-plotly.js/factory";
import * as Plotly from "plotly.js";
import React from "react";
import { v4 as uuidv4 } from "uuid";
import cx from "classnames";
import "./Plotting.scss";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";
import { SubExperimentWithMeasurements } from "../../store/types";

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

  const fourPLlines: Plotly.Data[] = subExperiments.map(([sex, color]) => {
    const sorted = sex.measurements
      .slice()
      .sort((x, y) => x.item.concentration - y.item.concentration);
    return {
      name: "4PL",
      x: sorted.map(a => a.item.concentration),
      y: sorted.map(a => fourPL(sex.meta.item.result, a.item.concentration)),
      type: "scatter",
      "xaxis.type": "log",
      mode: "lines",
      marker: { color },
    };
  });

  const plotlyData: Plotly.Data[] = subExperiments.map(([sex, color]) => {
    const sorted = sex.measurements
      .slice()
      .sort((x, y) => x.item.concentration - y.item.concentration);
    return {
      name: sex.meta.item.name,
      x: sorted.map(a => a.item.concentration),
      y: sorted.map(a => a.item.signal),
      type: "scatter",
      "xaxis.type": "log",
      mode: "markers",
      marker: { color },
    };
  });

  if (subExperiments.length === 0) {
    return (
      <EmptyPlaceholder
        className={className}
        title="Select sub-experiments to show them on chart"
      />
    );
  }

  return (
    <Plot
      key={chartKey}
      className={cx("compoundPlot", className)}
      data={plotlyData.concat(fourPLlines)}
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
          r: 0,
        },
      }}
      config={plotConfig}
    />
  );
}
