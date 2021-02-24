import React, { FunctionComponent, ReactElement, useRef, useState } from "react";
import { v4 as uuidv4 } from "uuid";
// TODO consider using smaller plotly bundle when we will optimize bundle size
// https://github.com/plotly/plotly.js/blob/master/dist/README.md#plotlyjs-basic
import Plot from "react-plotly.js";
import * as Plotly from "plotly.js";
import Api from "../../api/api";
import "./UploadPage.scss";
import { completed, failed, idle, isSucceeded, loading, RequestState } from "../../utils/request";
import { ExperimentalMeasurementDto } from "../../api/types";
import "../../components/Spinner.scss";

type CompoundMap = { [compoundId: string]: ExperimentalMeasurementDto[] };

function groupCompounds(mes: ExperimentalMeasurementDto[]): CompoundMap {
  return mes.reduce((rv: CompoundMap, x) => {
    // eslint-disable-next-line no-param-reassign
    (rv[x.compoundId] = rv[x.compoundId] || []).push(x);
    return rv;
  }, {});
}

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

function drawCompoundPlot(cmpd: string, points: ExperimentalMeasurementDto[]): React.ReactNode {
  const colors = ["red", "blue", "green"];
  const plotConfig: Partial<Plotly.Config> = {
    displaylogo: false,
    displayModeBar: false,
  };

  points.sort((x, y) => x.concentration - y.concentration);
  return (
    <Plot
      key={cmpd}
      className="compoundPlot"
      data={[
        {
          x: points.map(a => a.concentration),
          y: points.map(a => a.signal),
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

const UploadPage: FunctionComponent = (): ReactElement => {
  const uploadInputRef = useRef<HTMLInputElement | null>(null);
  const [uploadingStatus, setUploadingStatus] = useState<
    RequestState<ExperimentalMeasurementDto[]>
  >(idle());

  return (
    <div className="uploadPageContainer">
      <form
        className="uploadForm"
        onSubmit={async e => {
          e.preventDefault();
          try {
            if (
              uploadInputRef.current &&
              uploadInputRef.current.files &&
              uploadInputRef.current.files.length > 0
            ) {
              setUploadingStatus(loading());
              const file = uploadInputRef.current.files[0];
              const measurements = await Api.uploadExperiment(file);
              setUploadingStatus(completed(measurements));
            }
          } catch (ex) {
            setUploadingStatus(failed(ex.response.data));
          }
        }}
      >
        <input
          className="uploadForm__fileInput"
          type="file"
          ref={uploadInputRef}
          accept=".xlsx,.xls"
        />
        <button type="submit">Загрузить</button>
      </form>

      <div className="uploadPageResult">
        {isSucceeded(uploadingStatus) && (
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
                {uploadingStatus.result.map(v => (
                  <tr key={uuidv4()}>
                    <td>{v.compoundId}</td>
                    <td>{v.concentration}</td>
                    <td>{v.signal}</td>
                  </tr>
                ))}
              </tbody>
            </table>
            {Object.entries(groupCompounds(uploadingStatus.result)).map(([comp, meas]) => {
              return drawCompoundPlot(comp, meas);
            })}
          </>
        )}

        {uploadingStatus.status === "loading" && <div className="spinner">Uploading...</div>}

        {uploadingStatus.status === "failed" && (
          <span className="uploadResultFail">{uploadingStatus.error}</span>
        )}
      </div>
    </div>
  );
};

export default UploadPage;
