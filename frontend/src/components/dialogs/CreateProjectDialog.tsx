import React, { useState } from "react";
import { useSetRecoilState } from "recoil";
import { FormikErrors } from "formik";
import { modalDialogAtom } from "../../store/atoms";
import FormField from "../FormField/FormField";
import "../../styles/main.scss";
import "../DialogLayout/CreateDialog.scss";
import Api from "../../api/api";
import "../RoundSpinner.scss";
import { CreateDialogFooter, FormState } from "../DialogLayout/CreateDialogFooter";
import { toCreateProjectArgsApi } from "../../api/EdnaApi";
import { useProjectRefresher } from "../../store/updaters";
import { CreateProjectForm, toCreateProjectForm } from "./types";
import { ProjectDto } from "../../api/types";
import { DialogLayout } from "../DialogLayout/DialogLayout";

interface CreateProjectDialogProps {
  editing?: ProjectDto;
}

export function CreateProjectDialog({ editing }: CreateProjectDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const refreshProjects = useProjectRefresher();
  const [formState, setFormState] = useState<FormState>();

  return (
    <DialogLayout<CreateProjectForm>
      dialogClass="primaryDialogWindow"
      size="medium"
      title={editing ? "Edit project" : "Create project"}
      onClose={() => setModalDialog(undefined)}
      footer={<CreateDialogFooter formState={formState} editing={!!editing} cancelBtn />}
      formik={{
        initialValues: toCreateProjectForm(editing),
        validate: values => {
          const errors: FormikErrors<CreateProjectForm> = {};
          if (!values.name) {
            errors.name = "Name required";
          }
          return errors;
        },

        onSubmit: async form => {
          setFormState({ kind: "submitting" });
          try {
            if (editing) {
              await Api.editProject(editing.id, toCreateProjectArgsApi(form));
            } else {
              await Api.createProject(toCreateProjectArgsApi(form));
            }
            setModalDialog(undefined);
            refreshProjects();
          } catch (ex) {
            setFormState({ kind: "error", errorMsg: ex.message });
          }
        },
      }}
    >
      <FormField<string> required name="name" label="Name">
        {field => (
          <input
            className="ednaInput"
            value={field.value}
            onChange={e => field.onChange(e.target.value)}
          />
        )}
      </FormField>

      <FormField<string> name="description" label="Description">
        {field => (
          <textarea
            className="ednaTextarea createDialog__description"
            value={field.value}
            onChange={e => field.onChange(e.target.value)}
          />
        )}
      </FormField>
    </DialogLayout>
  );
}
