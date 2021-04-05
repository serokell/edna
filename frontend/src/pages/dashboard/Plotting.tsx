// TODO consider using smaller plotly bundle when we will optimize bundle size
// https://github.com/plotly/plotly.js/blob/master/dist/README.md#plotlyjs-basic
import * as Plotly from "plotly.js-basic-dist";
import React from "react";
import { v4 as uuidv4 } from "uuid";
import createPlotlyComponent from "react-plotly.js/factory";
import { SubExperimentWithMeasurements } from "../../store/types";
import "./Plotting.scss";

const plotConfig = {
  displaylogo: false,
  // displayModeBar: false,
};

interface PlotlyChartProps {
  subExperiments: SubExperimentWithMeasurements[];
}

export default function PlotlyChart({ subExperiments }: PlotlyChartProps): React.ReactElement {
  const chartKey = uuidv4();
  const Plot = createPlotlyComponent(Plotly);
  return (
    <Plot
      key={chartKey}
      className="compoundPlot"
      data={subExperiments.map(ex => {
        const sorted = ex.measurements.slice().sort((x, y) => x.concentration - y.concentration);
        return {
          name: ex.meta.item.name,
          x: sorted.map(a => a.concentration),
          y: sorted.map(a => a.signal),
          type: "scatter",
          "xaxis.type": "log",
          mode: "lines+markers",
          marker: { color: colors[Math.abs(hashCode(`${ex.meta.id}`)) % colors.length] },
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

const colors = ["red", "blue", "green", "yellow", "pink", "aqua", "chartreuse"];

// To determine compound color by its name.
// We want to have a compound the same color
// regardless of chart where it's plotted.
function hashCode(str: string) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const chr = str.charCodeAt(i);
    // eslint-disable-next-line no-bitwise
    hash = (hash << 5) - hash + chr;
    // eslint-disable-next-line no-bitwise
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
}
