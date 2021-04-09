import React, { FunctionComponent, ReactElement } from "react";
import { useRecoilState, useRecoilValueLoadable } from "recoil";
import { Form, Formik } from "formik";
import Api from "../../api/api";
import "../../components/Spinner/Spinner.scss";
import FormField from "../../components/FormField/FormField";
import "./UploadPage.scss";
import { excelFileAtom } from "../../store/atoms";
import CreatableSelect from "../../components/CreatableSelect";
import { isDefined, Maybe } from "../../utils/utils";
import UploadArea from "../../components/UploadArea/UploadArea";
import { isParsed } from "../../store/types";
import { UploadStatus } from "../../components/UploadStatus/UploadStatus";
import { UploadPreviewTable } from "../../components/UploadPreviewTable/UploadPreviewTable";
import { Button } from "../../components/Button/Button";
import PageLayout from "../../components/PageLayout/PageLayout";
import { UploadForm, uploadFormToApi } from "./uploadForm";
import { CreateMethodologyButton } from "../../components/buttons/CreateMethodologyButton";
import { CreateProjectButton } from "../../components/buttons/CreateProjectButton";
import { MethodologyDto, ProjectDto } from "../../api/types";
import { methodologiesQuery, projectsQuery } from "../../store/selectors";
import {
  useCompoundsRefresher,
  useFilteredExperimentsRefresher,
  useProjectRefresher,
  useTargetsRefresher,
} from "../../store/updaters";

export const UploadPage: FunctionComponent = (): ReactElement => {
  const projectsLoadable = useRecoilValueLoadable(projectsQuery);
  const methodologiesLoadable = useRecoilValueLoadable(methodologiesQuery);
  const [excelFile, setExcelFile] = useRecoilState(excelFileAtom);
  const projectsRefresher = useProjectRefresher();
  const targetsRefresher = useTargetsRefresher();
  const compoundsRefresher = useCompoundsRefresher();
  const filteredExperimentsRefresher = useFilteredExperimentsRefresher();

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
            if (apiType && excelFile?.state === "parsed") {
              await Api.uploadExperiments(apiType);
              setExcelFile({ state: "added", targets: excelFile.targets });
              projectsRefresher();
              targetsRefresher();
              compoundsRefresher();
              filteredExperimentsRefresher();
            }
          } catch (ex) {
            setExcelFile({ state: "failed-to-add", reason: ex.message });
          }
        }}
      >
        {({ resetForm }) => (
          <Form className="uploadingForm">
            <div className="uploadingForm__uploadTitle">Upload file</div>

            <div className="uploadingForm__uploadAddButtons">
              <CreateMethodologyButton />
              <CreateProjectButton />
            </div>

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

            {(excelFile?.state === "parsed" || excelFile?.state === "added") && (
              <UploadPreviewTable
                viewEnabled={excelFile.state === "added"}
                className="uploadingForm__previewTable"
                targets={excelFile.targets}
              />
            )}

            <FormField<Maybe<ProjectDto>>
              required
              name="project"
              label="Project"
              className="uploadingForm__project"
            >
              {field => (
                <CreatableSelect
                  optionsLoadable={projectsLoadable}
                  placeholder="Select a project"
                  placeholderEmpty="No projects"
                  toOption={proj => ({ value: `${proj.id}`, label: proj.item.name })}
                  tabIndex="2"
                  {...field}
                />
              )}
            </FormField>

            <FormField<Maybe<MethodologyDto>>
              required
              name="methodology"
              label="Methodology"
              className="uploadingForm__methodology"
            >
              {field => (
                <CreatableSelect
                  {...field}
                  optionsLoadable={methodologiesLoadable}
                  placeholder="Select a methodology"
                  placeholderEmpty="No methodologies"
                  toOption={meth => ({ value: `${meth.id}`, label: meth.item.name })}
                  tabIndex="3"
                />
              )}
            </FormField>

            <FormField<string>
              name="description"
              label="Description"
              className="uploadingForm__description"
            >
              {field => (
                <textarea
                  className="ednaTextarea uploadingForm__descriptionTextArea"
                  value={field.value}
                  onChange={e => field.onChange(e.target.value)}
                />
              )}
            </FormField>

            {isParsed(excelFile) && (
              <div className="uploadingForm__label">* marked will be created</div>
            )}

            <div className="uploadingForm__buttons">
              <Button
                type="primary"
                disabled={!isParsed(excelFile)}
                isSubmit
                className="uploadingForm__submitBtn"
                tabIndex={4}
              >
                Add experiments
              </Button>

              <Button
                type="text"
                tabIndex={5}
                onClick={() => {
                  setExcelFile(undefined);
                  resetForm();
                }}
              >
                Upload another one
              </Button>
            </div>
          </Form>
        )}
      </Formik>
    </PageLayout>
  );
};
