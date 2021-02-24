// TODO consider using smaller plotly bundle when we will optimize bundle size
// https://github.com/plotly/plotly.js/blob/master/dist/README.md#plotlyjs-basic
import Plot from "react-plotly.js";
import * as Plotly from "plotly.js";
import React from "react";
import { CompoundsMap, MeasurementDto } from "../../api/types";

const colors = ["red", "blue", "green"];
const plotConfig: Partial<Plotly.Config> = {
  displaylogo: false,
  // displayModeBar: false,
};

function hashCode(str: string) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const chr = str.charCodeAt(i);
    // eslint-disable-next-line no-bitwise
    hash = ((hash << 5) - hash) + chr;
    // eslint-disable-next-line no-bitwise
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
}

export function drawCompoundPlot(cmpd: string, points: MeasurementDto[]): React.ReactNode {
  points.sort((x, y) => x.concentration - y.concentration);
  return (
    <Plot
      key={cmpd}
      className="compoundPlot"
      data={[
        {
          x: points.map((a) => a.concentration),
          y: points.map((a) => a.signal),
          type: "scatter",
          mode: "lines+markers",
          marker: { color: colors[Math.abs(hashCode(cmpd)) % colors.length] },
        },
      ]}
      layout={{ title: cmpd }}
      config={plotConfig}
    />
  );
}

export function drawSeveralCompoundsPlot(compounds: CompoundsMap): React.ReactNode {
  const chartKey = Object.keys(compounds).join("-");
  return (
    <Plot
      key={chartKey}
      className="compoundPlot"
      data={Object.entries(compounds).map(([cmpd, meas]) => {
        meas.sort((x, y) => x.concentration - y.concentration);
        return {
          name: cmpd,
          x: meas.map((a) => a.concentration),
          y: meas.map((a) => a.signal),
          type: "scatter",
          mode: "lines+markers",
          marker: { color: colors[Math.abs(hashCode(cmpd)) % colors.length] },
        };
      })}
      layout={{ title: "Comparison chart" }}
      config={plotConfig}
    />
  );
}
