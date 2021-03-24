import React, { useState } from "react";
import { useSetRecoilState } from "recoil";
import { Form, Formik } from "formik";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import { modalDialogAtom } from "../../store/atoms";
import FormField from "../FormField/FormField";
import "../../styles/main.scss";
import "./CreateDialog.scss";
import Api from "../../api/api";
import { CreateDialogFooter, FormState } from "./CreateDialogFooter";
import { CreateMethodologyArgsApi } from "../../api/EdnaApi";
import { replaceEmptyWithUndefined } from "../../utils/utils";
import { useMethodologiesRefresher } from "../../store/updaters";

interface CreateMethodologyForm {
  name: string;
  description: string;
  confluence: string;
}

function toApiArgs(form: CreateMethodologyForm): CreateMethodologyArgsApi {
  return {
    name: form.name,
    description: replaceEmptyWithUndefined(form.description.trim()),
    confluence: replaceEmptyWithUndefined(form.confluence.trim()),
  };
}

export function CreateMethodologyDialog(): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const refreshMethodologies = useMethodologiesRefresher();
  const [formState, setFormState] = useState<FormState>();

  return (
    <DialogLayout title="Create methodology" onClose={() => setModalDialog(undefined)}>
      <Formik<CreateMethodologyForm>
        initialValues={{
          name: "",
          description: "",
          confluence: "",
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
            await Api.createMethodology(toApiArgs(form));
            setModalDialog(undefined);
            refreshMethodologies();
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

          <FormField<string> name="confluence" label="Confluence link">
            {field => (
              <input
                className="ednaInput"
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
