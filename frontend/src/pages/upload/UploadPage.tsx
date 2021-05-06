// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement, useState } from "react";
import { useRecoilState, useRecoilValueLoadable } from "recoil";
import { Form, Formik } from "formik";
import { Prompt } from "react-router-dom";
import Api from "../../api/api";
import "../../components/Spinner/Spinner.scss";
import FormField from "../../components/FormField/FormField";
import "./UploadPage.scss";
import { excelFileAtom } from "../../store/atoms";
import Combobox from "../../components/Combobox";
import { isDefined, Maybe } from "../../utils/utils";
import UploadArea from "../../components/UploadArea/UploadArea";
import { isAdded, isParsed } from "../../store/types";
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
  useMethodologiesRefresher,
  useNotificationListUpdater,
  useProjectsRefresher,
  useTargetsRefresher,
} from "../../store/updaters";

export const UploadPage: FunctionComponent = (): ReactElement => {
  // TODO make it async
  const projectsLoadable = useRecoilValueLoadable(projectsQuery({}));
  const methodologiesLoadable = useRecoilValueLoadable(methodologiesQuery({}));
  const [excelFile, setExcelFile] = useRecoilState(excelFileAtom);
  const projectsRefresher = useProjectsRefresher();
  const targetsRefresher = useTargetsRefresher();
  const compoundsRefresher = useCompoundsRefresher();
  const filteredExperimentsRefresher = useFilteredExperimentsRefresher();
  const methodologiesRefresher = useMethodologiesRefresher();
  const notificationsUpdater = useNotificationListUpdater();
  const [currentProject, setCurrentProject] = useState<Maybe<ProjectDto>>(undefined);
  const [hasUnsavedFields, setHasUnsavedFields] = useState(false);
  const showPrompt = (ch: React.ReactNode) => (
    <>
      <Prompt
        when={hasUnsavedFields}
        message={loc => {
          if (loc.pathname === "/upload") {
            return true;
          }
          return "All unsaved data will be lost if you leave the page. Do you really want to leave the page?";
        }}
      />
      {ch}
    </>
  );

  return showPrompt(
    <PageLayout>
      <Formik<UploadForm>
        initialValues={{
          file: undefined,
          project: undefined,
          methodology: undefined,
          description: "",
        }}
        validate={values => {
          setHasUnsavedFields(
            !!values.file || !!values.project || !!values.methodology || !!values.description
          );
          setCurrentProject(values.project);
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
              const targets = await Api.uploadExperiments(apiType);
              setExcelFile({ state: "added", targets });
              setHasUnsavedFields(false);
              projectsRefresher();
              targetsRefresher();
              compoundsRefresher();
              methodologiesRefresher();
              filteredExperimentsRefresher();
            }
          } catch (ex) {
            setExcelFile({ state: "failed-to-add", reason: ex.message });
          }
        }}
      >
        {({ resetForm, values, setValues }) => (
          <Form className="uploadingForm">
            <div className="uploadingForm__uploadTitle">Upload file</div>

            <div className="uploadingForm__uploadAddButtons">
              <CreateMethodologyButton />
              <CreateProjectButton />
            </div>

            <FormField<File>
              name="file"
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
                projectId={excelFile.state === "added" ? currentProject?.id : undefined}
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
                <Combobox
                  optionsLoadable={projectsLoadable}
                  placeholder="Select a project"
                  placeholderEmpty="No projects"
                  toOption={proj => ({ value: `${proj.id}`, label: proj.item.name })}
                  tabIndex="2"
                  isDisabled={isAdded(excelFile)}
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
                <Combobox
                  {...field}
                  optionsLoadable={methodologiesLoadable}
                  placeholder="Select a methodology"
                  placeholderEmpty="No methodologies"
                  toOption={meth => ({ value: `${meth.id}`, label: meth.item.name })}
                  tabIndex="3"
                  isDisabled={isAdded(excelFile)}
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
                  disabled={isAdded(excelFile)}
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
                disabled={!isAdded(excelFile) && !hasUnsavedFields}
                onClick={() => {
                  setHasUnsavedFields(false);
                  setExcelFile(undefined);
                  resetForm();
                  notificationsUpdater({
                    type: "Add",
                    notificationType: "Error",
                    element: manualRemove => (
                      <div className="uploadingForm__resetNotify">
                        <span>You have reset all completed fields</span>
                        <Button
                          type="text"
                          className="uploadingForm__resetNotifyBtn"
                          disabled={!isAdded(excelFile) && !hasUnsavedFields}
                          onClick={() => {
                            setHasUnsavedFields(true);
                            setValues(values);
                            setExcelFile(excelFile);
                            manualRemove();
                          }}
                        >
                          Undo
                        </Button>
                      </div>
                    ),
                  });
                }}
              >
                {isAdded(excelFile) ? "Upload another one" : "Reset"}
              </Button>
            </div>
          </Form>
        )}
      </Formik>
    </PageLayout>
  );
};
