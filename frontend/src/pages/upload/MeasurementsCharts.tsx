import React from "react";
import { v4 as uuidv4 } from "uuid";
import { groupCompounds, MeasurementDto } from "../../api/types";
import PlotlyChart from "./Plotting";
import "./MeasurementsCharts.scss";

interface MeasurementsChartsProps {
  measurements: MeasurementDto[];
}

export default function MeasurementsCharts({
  measurements,
}: MeasurementsChartsProps): React.ReactElement {
  const grouped = groupCompounds(measurements);
  return (
    <>
      <table className="uploadResultTable">
        <thead>
          <tr>
            <th>Compound</th>
            <th>Target</th>
            <th>Concentration</th>
            <th>Signal</th>
            <th>Outlier</th>
          </tr>
        </thead>

        <tbody>
          {measurements.map(v => (
            <tr key={uuidv4()}>
              <td>{v.compoundId}</td>
              <td>{v.targetId}</td>
              <td>{v.concentration}</td>
              <td>{v.signal}</td>
              <td>{v.outlier.toString()}</td>
            </tr>
          ))}
        </tbody>
      </table>

      {Object.entries(grouped).map(([comp, meas]) => (
        <PlotlyChart key={uuidv4()} compounds={{ [comp]: meas }} />
      ))}
      <PlotlyChart compounds={grouped} />
    </>
  );
}
