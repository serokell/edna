// TODO consider using smaller plotly bundle when we will optimize bundle size
// https://github.com/plotly/plotly.js/blob/master/dist/README.md#plotlyjs-basic
import * as Plotly from "plotly.js-basic-dist";
import React from "react";
import { v4 as uuidv4 } from "uuid";
import createPlotlyComponent from "react-plotly.js/factory";
import cx from "classnames";
import { SubExperimentWithMeasurements } from "../../store/types";
import "./Plotting.scss";

const plotConfig = {
  displaylogo: false,
  // displayModeBar: false,
};

interface PlotlyChartProps {
  className?: string;
  subExperiments: [SubExperimentWithMeasurements, string][];
}

export default function PlotlyChart({
  subExperiments,
  className,
}: PlotlyChartProps): React.ReactElement {
  const chartKey = uuidv4();
  const Plot = createPlotlyComponent(Plotly);

  return (
    <Plot
      key={chartKey}
      className={cx("compoundPlot", className)}
      data={subExperiments.map(([sex, color]) => {
        const sorted = sex.measurements.slice().sort((x, y) => x.concentration - y.concentration);
        return {
          name: sex.meta.item.name,
          x: sorted.map(a => a.concentration),
          y: sorted.map(a => a.signal),
          type: "scatter",
          "xaxis.type": "log",
          mode: "lines+markers",
          marker: { color },
        };
      })}
      layout={{
        xaxis: {
          type: "log",
          autorange: true,
        },
      }}
      config={plotConfig}
    />
  );
}
