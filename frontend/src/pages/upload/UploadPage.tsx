import React, { FunctionComponent, ReactElement, useState } from "react";
import { useRecoilValueLoadable } from "recoil";
import Api from "../../api/api";
import { completed, failed, idle, isSucceeded, loading, RequestState } from "../../utils/request";
import { MeasurementDto } from "../../api/types";
import "../../components/Spinner.scss";
import MeasurementsCharts from "./MeasurementsCharts";
import Header from "../../components/Header/Header";
import FormField from "../../components/FormField/FormField";
import "./UploadPage.scss";
import UploadArea from "../../components/UploadArea/UploadArea";
import { methodologiesAtom, projectsAtom } from "../../store/atoms";
import CreatableSelect from "../../components/CreatableSelect";
import { createMethodologyUpdater, createProjectUpdater } from "../../store/updaters";

const UploadPage: FunctionComponent = (): ReactElement => {
  const [chosenFile, setChosenFile] = useState<File | undefined>(undefined);
  const [uploadingStatus, setUploadingStatus] = useState<RequestState<MeasurementDto[]>>(idle());

  // const targets = [
  //   { value: "cancer", label: "Cancer" },
  //   { value: "pneumonia", label: "Pneumonia" },
  // ];

  const createProject = createProjectUpdater();
  const createMethodology = createMethodologyUpdater();

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
            label="Project"
            control={
              <CreatableSelect
                optionsLoadable={useRecoilValueLoadable(projectsAtom)}
                placeholder="Select a project"
                placeholderNo="No projects"
                toOption={proj => ({ value: `${proj.projectId}`, label: proj.name })}
                optionCreator={async newProjName => {
                  try {
                    return await createProject(newProjName);
                    // TODO show success notification
                  } catch (e) {
                    // TODO show failed notification
                    console.error(e);
                    return undefined;
                  }
                }}
                tabIndex="2"
              />
            }
          />

          <FormField
            label="Methodology"
            control={
              <CreatableSelect
                optionsLoadable={useRecoilValueLoadable(methodologiesAtom)}
                placeholder="Select a methodology"
                placeholderNo="No methodologies"
                toOption={meth => ({ value: `${meth.methodologyId}`, label: meth.name })}
                optionCreator={async newMethName => {
                  try {
                    return await createMethodology(newMethName);
                    // TODO show success notification
                  } catch (e) {
                    // TODO show failed notification
                    console.error(e);
                    return undefined;
                  }
                }}
                tabIndex="3"
              />
            }
          />

          <FormField
            label="Description"
            control={<textarea className="ednaTextarea uploadingForm__description" tabIndex={4} />}
          />

          <button className="ednaButton uploadingForm__submitBtn" type="submit" tabIndex={5}>
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
