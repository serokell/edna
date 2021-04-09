// TODO consider using smaller plotly bundle when we will optimize bundle size
// https://github.com/plotly/plotly.js/blob/master/dist/README.md#plotlyjs-basic
import * as Plotly from "plotly.js-basic-dist";
import React from "react";
import { v4 as uuidv4 } from "uuid";
import createPlotlyComponent from "react-plotly.js/factory";
import cx from "classnames";
import { SubExperimentWithMeasurements } from "../../store/types";
import "./Plotting.scss";
import { EmptyPlaceholder } from "../../components/EmptyPlaceholder/EmptyPlaceholder";

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
      data={subExperiments.map(([sex, color]) => {
        const sorted = sex.measurements
          .slice()
          .sort((x, y) => x.item.concentration - y.item.concentration);
        return {
          name: sex.meta.item.name,
          x: sorted.map(a => a.item.concentration),
          y: sorted.map(a => a.item.signal),
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
