import React, { FunctionComponent, ReactElement, useRef, useState } from "react";
import { v4 as uuidv4 } from "uuid";
import Api from "../../api/api";
import "./UploadPage.scss";
import { completed, failed, idle, isSucceeded, loading, RequestState } from "../../utils/request";
import { groupCompounds, MeasurementDto } from "../../api/types";
import "../../components/Spinner.scss";
import { drawCompoundPlot, drawSeveralCompoundsPlot } from "./Plotting";

const UploadPage: FunctionComponent = (): ReactElement => {
  const uploadInputRef = useRef<HTMLInputElement | null>(null);
  const [uploadingStatus, setUploadingStatus] = useState<RequestState<MeasurementDto[]>>(idle());

  function renderSucceeded(measurements: MeasurementDto[]): React.ReactNode {
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
        {Object.entries(grouped).map(([comp, meas]) => drawCompoundPlot(comp, meas))}
        {drawSeveralCompoundsPlot(grouped)}
      </>
    );
  }

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
        {isSucceeded(uploadingStatus) && renderSucceeded(uploadingStatus.result)}

        {uploadingStatus.status === "loading" && <div className="spinner">Uploading...</div>}

        {uploadingStatus.status === "failed" && (
          <span className="uploadResultFail">{uploadingStatus.error}</span>
        )}
      </div>
    </div>
  );
};

export default UploadPage;
