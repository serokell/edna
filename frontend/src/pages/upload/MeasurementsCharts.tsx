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
      <table className="uploadResultTable">
        <thead>
          <tr>
            <th>Compound</th>
            <th>Concentration</th>
            <th>Signal</th>
            <th>Target</th>
          </tr>
        </thead>

        <tbody>
          {experiments.map(ex =>
            ex.measurements.map(v => (
              <tr key={uuidv4()}>
                <td>{v.compoundId}</td>
                <td>{v.concentration}</td>
                <td>{v.signal}</td>
                <td>{v.signal}</td>
              </tr>
            ))
          )}
        </tbody>
      </table>

      {experiments.map(x => (
        <PlotlyChart key={uuidv4()} experiments={[x]} />
      ))}
      <PlotlyChart experiments={experiments} />
    </>
  );
}
