import React, { FunctionComponent, ReactElement } from "react";
import { useRecoilState, useRecoilValueLoadable } from "recoil";
import { Form, Formik } from "formik";
import Api from "../../api/api";
import "../../components/Spinner/Spinner.scss";
import MeasurementsCharts from "./MeasurementsCharts";
import FormField from "../../components/FormField/FormField";
import "./UploadPage.scss";
import { methodologiesAtom, excelFileAtom, projectsAtom } from "../../store/atoms";
import CreatableSelect from "../../components/CreatableSelect";
import { createMethodologyUpdater, createProjectUpdater } from "../../store/updaters";
import { isDefined, Maybe } from "../../utils/utils";
import UploadArea from "../../components/UploadArea/UploadArea";
import { isParsed, Methodology, Project } from "../../store/types";
import { UploadStatus } from "../../components/UploadStatus/UploadStatus";
import { UploadPreviewTable } from "../../components/UploadPreviewTable/UploadPreviewTable";
import { Button } from "../../components/Button/Button";
import PageLayout from "../../components/PageLayout/PageLayout";
import { UploadForm, uploadFormToApi } from "./uploadForm";

export const UploadPage: FunctionComponent = (): ReactElement => {
  const createProject = createProjectUpdater();
  const createMethodology = createMethodologyUpdater();
  const projectsLoadable = useRecoilValueLoadable(projectsAtom);
  const methodologiesLoadable = useRecoilValueLoadable(methodologiesAtom);
  const [excelFile, setExcelFile] = useRecoilState(excelFileAtom);

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
            const apiType = uploadFormToApi(form);
            if (apiType) {
              await Api.uploadExperiments(apiType);
              setExcelFile({ state: "added" });
            }
          } catch (ex) {
            setExcelFile({ state: "failed-to-add", reason: ex.message });
          }
        }}
      >
        <Form className="uploadingForm">
          <div className="uploadingForm__uploadTitle">Uploading file</div>

          <FormField<File>
            name="file"
            label="File"
            className={
              isDefined(excelFile)
                ? "uploadingForm__uploadArea_hidden"
                : "uploadingForm__uploadArea"
            }
          >
            {field => <UploadArea {...field} />}
          </FormField>

          {isDefined(excelFile) && <UploadStatus />}

          {excelFile?.state === "parsed" && (
            <UploadPreviewTable
              className="uploadingForm__previewTable"
              targets={excelFile.targets}
            />
          )}

          <FormField<Maybe<Project>>
            name="project"
            label="Project"
            className="uploadingForm__project"
          >
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

          <FormField<Maybe<Methodology>>
            name="methodology"
            label="Methodology"
            className="uploadingForm__methodology"
          >
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

          {isParsed(excelFile) && (
            <div className="uploadingForm__label">* marked will be created</div>
          )}

          <div className="uploadingForm__buttons">
            <Button
              disabled={!isParsed(excelFile)}
              type="submit"
              className="uploadingForm__submitBtn"
              tabIndex={4}
            >
              Add experiments
            </Button>

            <Button
              type="outlined"
              className="uploadingForm__anotherBtn"
              tabIndex={5}
              onClick={() => {
                setExcelFile(undefined);
              }}
            >
              Upload another one
            </Button>
          </div>
        </Form>
      </Formik>

      <div className="uploadResult">
        {isParsed(excelFile) && <MeasurementsCharts experiments={excelFile.experiments} />}

        {(excelFile?.state === "uploading" || excelFile?.state === "verifying") && (
          <div className="spinner">Uploading...</div>
        )}

        {(excelFile?.state === "failed-to-parse" || excelFile?.state === "failed-to-add") && (
          <span className="uploadResult__fail">{excelFile.reason}</span>
        )}
      </div>
    </PageLayout>
  );
};
