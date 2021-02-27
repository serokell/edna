import React, { FunctionComponent, ReactElement, useState } from "react";
import Api from "../../api/api";
import { completed, failed, idle, isSucceeded, loading, RequestState } from "../../utils/request";
import { MeasurementDto } from "../../api/types";
import "../../components/Spinner.scss";
import MeasurementsCharts from "./MeasurementsCharts";
import Header from "../../components/Header/Header";
import FormField from "../../components/FormField/FormField";
import "./UploadPage.scss";
import UploadArea from "../../components/UploadArea/UploadArea";

const UploadPage: FunctionComponent = (): ReactElement => {
  const [chosenFile, setChosenFile] = useState<File | undefined>(undefined);

  const [uploadingStatus, setUploadingStatus] = useState<RequestState<MeasurementDto[]>>(idle());

  return (
    <>
      <Header />
      <div className="container">
        <div className="uploadTitle">Uploading file</div>

        <form
          className="uploadingForm"
          onSubmit={async e => {
            e.preventDefault();
            try {
              if (chosenFile) {
                setUploadingStatus(loading());
                const measurements = await Api.uploadExperiment(chosenFile);
                setUploadingStatus(completed(measurements));
              }
            } catch (ex) {
              setUploadingStatus(failed(ex.response.data));
            }
          }}
        >
          <FormField
            label="File"
            className="uploadingForm__uploadArea"
            control={<UploadArea chosenFile={chosenFile} onFileChosen={f => setChosenFile(f)} />}
          />

          <FormField label="Methodology" control={<input />} />

          <FormField label="Target" control={<input />} />

          <FormField className="uploadingForm__nameField" label="Name" control={<input />} />

          <FormField
            label="Description"
            control={<textarea className="uploadingForm__description" />}
          />

          <button className="uploadingForm__submitBtn" type="submit">
            Save
          </button>
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
    </>
  );
};

export default UploadPage;
