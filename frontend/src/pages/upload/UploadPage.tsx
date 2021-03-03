import React, { FunctionComponent, ReactElement, useRef, useState } from "react";
import Api from "../../api/api";
import "./UploadPage.scss";
import { completed, failed, idle, isSucceeded, loading, RequestState } from "../../utils/request";
import { MeasurementDto } from "../../api/types";
import "../../components/Spinner.scss";
import MeasurementsCharts from "./MeasurementsCharts";

const UploadPage: FunctionComponent = (): ReactElement => {
  const uploadInputRef = useRef<HTMLInputElement | null>(null);
  const [uploadingStatus, setUploadingStatus] = useState<RequestState<MeasurementDto[]>>(idle());

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
          <MeasurementsCharts measurements={uploadingStatus.result} />
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
