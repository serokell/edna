import React, { useState } from "react";
import { useSetRecoilState } from "recoil";
import { Form, Formik } from "formik";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import { modalDialogAtom, projectsAtom } from "../../store/atoms";
import FormField from "../FormField/FormField";
import "../../styles/main.scss";
import "./CreateDialog.scss";
import Api from "../../api/api";
import "../RoundSpinner.scss";
import { CreateDialogFooter, FormState } from "./CreateDialogFooter";
import { CreateProjectArgsApi } from "../../api/EdnaApi";
import { replaceEmptyWithUndefined } from "../../utils/utils";

interface CreateProjectForm {
  name: string;
  description: string;
}

function toApiArgs(form: CreateProjectForm): CreateProjectArgsApi {
  return {
    name: form.name,
    description: replaceEmptyWithUndefined(form.description.trim()),
  };
}

export function CreateProjectDialog(): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const setProjects = useSetRecoilState(projectsAtom);
  const [formState, setFormState] = useState<FormState>();

  return (
    <DialogLayout title="Create project" onClose={() => setModalDialog(undefined)}>
      <Formik<CreateProjectForm>
        initialValues={{
          name: "",
          description: "",
        }}
        validate={values => {
          const errors: any = {};
          if (!values.name) {
            errors.name = "Name required";
          }
          return errors;
        }}
        onSubmit={async form => {
          setFormState({ kind: "submitting" });
          try {
            const newProject = await Api.createProject(toApiArgs(form));
            setModalDialog(undefined);
            setProjects(projects => projects.concat([newProject]));
            // TODO update list here
          } catch (ex) {
            setFormState({ kind: "error", errorMsg: ex.message });
          }
        }}
      >
        <Form>
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

          <CreateDialogFooter formState={formState} />
        </Form>
      </Formik>
    </DialogLayout>
  );
}
