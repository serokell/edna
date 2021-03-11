import React, { FunctionComponent, ReactElement, useState } from "react";
import { useRecoilValueLoadable } from "recoil";
import { Form, Formik } from "formik";
import Api from "../../api/api";
import { completed, failed, idle, isSucceeded, loading, RequestState } from "../../utils/request";
import { MeasurementDto } from "../../api/types";
import "../../components/Spinner.scss";
import MeasurementsCharts from "./MeasurementsCharts";
import FormField from "../../components/FormField/FormField";
import "./UploadPage.scss";
import { methodologiesAtom, projectsAtom } from "../../store/atoms";
import CreatableSelect from "../../components/CreatableSelect";
import { createMethodologyUpdater, createProjectUpdater } from "../../store/updaters";
import { isDefined, Maybe } from "../../utils/utils";
import UploadArea from "../../components/UploadArea/UploadArea";
import { Methodology, Project } from "../../store/types";
import PageLayout from "../../components/PageLayout/PageLayout";

interface UploadForm {
  file: Maybe<File>;
  methodology: Maybe<Methodology>;
  project: Maybe<Project>;
  description: string;
}

const UploadPage: FunctionComponent = (): ReactElement => {
  const [uploadingStatus, setUploadingStatus] = useState<RequestState<MeasurementDto[]>>(idle());
  const createProject = createProjectUpdater();
  const createMethodology = createMethodologyUpdater();
  const projectsLoadable = useRecoilValueLoadable(projectsAtom);
  const methodologiesLoadable = useRecoilValueLoadable(methodologiesAtom);

  return (
    <PageLayout>
      <Formik<UploadForm>
        initialValues={{
          file: undefined,
          project: undefined,
          methodology: undefined,
          description: "",
        }}
        validate={values => {
          const errors: any = {};
          if (!isDefined(values.file)) {
            errors.file = "File required";
          }
          if (!isDefined(values.project)) {
            errors.project = "Project required";
          }
          if (!isDefined(values.methodology)) {
            errors.methodology = "Methodology required";
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

          <FormField<Maybe<Project>> name="project" label="Project">
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

          <FormField<Maybe<Methodology>> name="methodology" label="Methodology">
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

          <FormField
            as="textarea"
            name="description"
            label="Description"
            className="uploadingForm__description"
            classNameInner="ednaTextarea"
            tabIndex={4}
          />

          <button className="primaryButton uploadingForm__submitBtn" type="submit" tabIndex={5}>
            Save
          </button>
        </Form>
      </Formik>

      <div className="uploadResult">
        {isSucceeded(uploadingStatus) && (
          <MeasurementsCharts measurements={uploadingStatus.result} />
        )}

        {uploadingStatus.status === "loading" && <div className="spinner">Uploading...</div>}

        {uploadingStatus.status === "failed" && (
          <span className="uploadResult__fail">{uploadingStatus.error}</span>
        )}
      </div>
    </PageLayout>
  );
};

export default UploadPage;
