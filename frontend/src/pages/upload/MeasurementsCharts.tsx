import React from "react";
import { v4 as uuidv4 } from "uuid";
import PlotlyChart from "./Plotting";
import "./MeasurementsCharts.scss";
import { Experiment } from "../../store/types";

interface MeasurementsChartsProps {
  experiments: Experiment[];
}

export default function MeasurementsCharts({
  experiments,
}: MeasurementsChartsProps): React.ReactElement {
  return (
    <>
      {experiments.map(x => (
        <PlotlyChart key={uuidv4()} experiments={[x]} title={`${x.compoundId} ${x.target}`} />
      ))}
      <PlotlyChart experiments={experiments} title="Comparison chart" />
    </>
  );
}
