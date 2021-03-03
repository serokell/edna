import React from "react";
import { v4 as uuidv4 } from "uuid";
import { groupCompounds, MeasurementDto } from "../../api/types";
import PlotlyChart from "./Plotting";

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
            <th>Concentration</th>
            <th>Signal</th>
          </tr>
        </thead>

        <tbody>
          {measurements.map(v => (
            <tr key={uuidv4()}>
              <td>{v.compoundId}</td>
              <td>{v.concentration}</td>
              <td>{v.signal}</td>
            </tr>
          ))}
        </tbody>
      </table>

      {Object.entries(grouped).map(([comp, meas]) => (
        <PlotlyChart compounds={{ [comp]: meas }} />
      ))}
      <PlotlyChart compounds={grouped} />
    </>
  );
}
