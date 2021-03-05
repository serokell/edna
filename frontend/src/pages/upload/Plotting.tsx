// TODO consider using smaller plotly bundle when we will optimize bundle size
// https://github.com/plotly/plotly.js/blob/master/dist/README.md#plotlyjs-basic
import Plot from "react-plotly.js";
import * as Plotly from "plotly.js";
import React from "react";
import { v4 as uuidv4 } from "uuid";
import { CompoundsMap } from "../../api/types";
import "./Plotting.scss";

const colors = ["red", "blue", "green", "yellow", "pink", "aqua", "chartreuse"];
const plotConfig: Partial<Plotly.Config> = {
  displaylogo: false,
  // displayModeBar: false,
};

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

interface PlotlyChartProps {
  compounds: CompoundsMap;
}

export default function PlotlyChart({ compounds }: PlotlyChartProps): React.ReactElement {
  const chartKey = uuidv4();
  return (
    <Plot
      key={chartKey}
      className="compoundPlot"
      data={Object.entries(compounds).map(([cmpd, meas]) => {
        meas.sort((x, y) => x.concentration - y.concentration);
        return {
          name: cmpd,
          x: meas.map(a => a.concentration),
          y: meas.map(a => a.signal),
          type: "scatter",
          "xaxis.type": "log",
          mode: "lines+markers",
          marker: { color: colors[Math.abs(hashCode(cmpd)) % colors.length] },
        };
      })}
      layout={{
        title: "Comparison chart",
        xaxis: {
          type: "log",
          autorange: true,
        },
      }}
      config={plotConfig}
    />
  );
}
