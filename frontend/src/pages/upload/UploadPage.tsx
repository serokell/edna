import React, { FunctionComponent, ReactElement, useState } from "react";
import { useRecoilValueLoadable } from "recoil";
import { Form, Formik } from "formik";
import Api from "../../api/api";
import { completed, failed, idle, isSucceeded, loading, RequestState } from "../../utils/request";
import { MeasurementDto } from "../../api/types";
import "../../components/Spinner.scss";
import MeasurementsCharts from "./MeasurementsCharts";
import Header from "../../components/Header/Header";
import FormField from "../../components/FormField/FormField";
import "./UploadPage.scss";
import { methodologiesAtom, projectsAtom } from "../../store/atoms";
import CreatableSelect from "../../components/CreatableSelect";
import { createMethodologyUpdater, createProjectUpdater } from "../../store/updaters";
import { isDefined, Maybe } from "../../utils/utils";
import UploadArea from "../../components/UploadArea/UploadArea";

interface UploadForm {
  file: Maybe<File>;
  methodologyId: Maybe<number>;
  projectId: Maybe<number>;
  description: string;
}

const UploadPage: FunctionComponent = (): ReactElement => {
  const [uploadingStatus, setUploadingStatus] = useState<RequestState<MeasurementDto[]>>(idle());
  const createProject = createProjectUpdater();
  const createMethodology = createMethodologyUpdater();
  const projectsLoadable = useRecoilValueLoadable(projectsAtom);
  const methodologiesLoadable = useRecoilValueLoadable(methodologiesAtom);

  return (
    <>
      <Header />
      <div className="container">
        <Formik<UploadForm>
          initialValues={{
            file: undefined,
            projectId: undefined,
            methodologyId: undefined,
            description: "",
          }}
          validate={values => {
            console.log(values);
            const errors: any = {};
            if (!isDefined(values.file)) {
              errors.file = "File required";
            }
            if (!isDefined(values.projectId)) {
              errors.projectId = "Project required";
            }
            if (!isDefined(values.methodologyId)) {
              errors.methodologyId = "Methodology required";
            }
            return errors;
          }}
          onSubmit={async form => {
            try {
              if (form.file) {
                setUploadingStatus(loading());
                // TODO pass other fields
                const measurements = await Api.uploadExperiment(form.file);
                setUploadingStatus(completed(measurements));
              }
            } catch (ex) {
              setUploadingStatus(failed(ex.response.data));
            }
          }}
        >
          <Form className="uploadingForm">
            <div className="uploadingForm__uploadTitle">Uploading file</div>

            <FormField<File> name="file" label="File" className="uploadingForm__uploadArea">
              {field => <UploadArea {...field} />}
            </FormField>

            <FormField name="projectId" label="Project">
              {field => (
                <CreatableSelect
                  optionsLoadable={projectsLoadable}
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
                  {...field}
                />
              )}
            </FormField>

            <FormField name="methodologyId" label="Methodology">
              {field => (
                <CreatableSelect
                  {...field}
                  optionsLoadable={methodologiesLoadable}
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
              )}
            </FormField>

            <FormField<string> name="description" label="Description">
              {field => (
                <textarea
                  className="ednaTextarea uploadingForm__description"
                  tabIndex={4}
                  {...field}
                />
              )}
            </FormField>

            <button className="ednaButton uploadingForm__submitBtn" type="submit" tabIndex={5}>
              Save
            </button>
          </Form>
        </Formik>

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
