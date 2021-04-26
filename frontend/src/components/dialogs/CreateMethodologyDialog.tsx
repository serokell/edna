// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { useState } from "react";
import { useSetRecoilState } from "recoil";
import { FormikErrors } from "formik";
import { modalDialogAtom } from "../../store/atoms";
import FormField from "../FormField/FormField";
import "../../styles/main.scss";
import "../DialogLayout/CreateDialog.scss";
import Api from "../../api/api";
import { CreateDialogFooter, FormState } from "../DialogLayout/CreateDialogFooter";
import { useMethodologiesRefresher } from "../../store/updaters";
import { CreateMethodologyForm, toCreateMethodologyForm } from "./types";
import { toCreateMethodologyArgsApi } from "../../api/EdnaApi";
import { MethodologyDto } from "../../api/types";
import { DialogLayout } from "../DialogLayout/DialogLayout";

interface CreateMethodologyDialogProps {
  editing?: MethodologyDto;
}

export function CreateMethodologyDialog({
  editing,
}: CreateMethodologyDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const refreshMethodologies = useMethodologiesRefresher();
  const [formState, setFormState] = useState<FormState>();

  return (
    <DialogLayout<CreateMethodologyForm>
      dialogClass="primaryDialogWindow"
      size="medium"
      title={editing ? "Edit methodology" : "Create methodology"}
      onClose={() => setModalDialog(undefined)}
      footer={<CreateDialogFooter formState={formState} editing={!!editing} cancelBtn />}
      formik={{
        initialValues: toCreateMethodologyForm(editing),
        validate: values => {
          const errors: FormikErrors<CreateMethodologyForm> = {};
          if (!values.name) {
            errors.name = "Name required";
          }
          return errors;
        },

        onSubmit: async form => {
          setFormState({ kind: "submitting" });
          try {
            if (editing) {
              await Api.editMethodology(editing.id, toCreateMethodologyArgsApi(form));
            } else {
              await Api.createMethodology(toCreateMethodologyArgsApi(form));
            }
            setModalDialog(undefined);
            refreshMethodologies();
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

      <FormField<string> name="confluence" label="Confluence link">
        {field => (
          <input
            className="ednaInput"
            value={field.value}
            onChange={e => field.onChange(e.target.value)}
          />
        )}
      </FormField>
    </DialogLayout>
  );
}
