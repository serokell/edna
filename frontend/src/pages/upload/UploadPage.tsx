import React, { FunctionComponent, ReactElement, useState } from "react";
import Select from "react-select/creatable";
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

  const methodologies = [
    { value: "meth1", label: "Meth1" },
    { value: "meth2", label: "Meth2" },
    { value: "meth3", label: "Meth3" },
  ];

  const targets = [
    { value: "cancer", label: "Cancer" },
    { value: "pneumonia", label: "Pneumonia" },
  ];

  const projects = [
    { value: "project1", label: "Project1" },
    { value: "project2", label: "Project2" },
  ];

  return (
    <>
      <Header />
      <div className="container">
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
          <div className="uploadingForm__uploadTitle">Uploading file</div>

          <FormField
            label="File"
            className="uploadingForm__uploadArea"
            control={<UploadArea chosenFile={chosenFile} onFileChosen={f => setChosenFile(f)} />}
          />

          <FormField
            className="uploadingForm__nameField"
            label="Name"
            control={<input className="endaInput" tabIndex={1} />}
          />

          <FormField
            label="Project"
            control={
              <Select tabIndex="2" isClearable placeholder="Select a project" options={projects} />
            }
          />

          <FormField
            label="Methodology"
            control={
              <Select
                tabIndex="4"
                isClearable
                placeholder="Select a methodology"
                options={methodologies}
              />
            }
          />

          <FormField
            label="Target"
            control={
              <Select tabIndex="3" isClearable placeholder="Select a target" options={targets} />
            }
          />

          <FormField
            label="Description"
            control={<textarea className="ednaTextarea uploadingForm__description" tabIndex={5} />}
          />

          <button className="ednaButton uploadingForm__submitBtn" type="submit" tabIndex={6}>
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
