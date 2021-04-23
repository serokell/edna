// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { useState } from "react";
import { useRecoilCallback, useSetRecoilState } from "recoil";
import { modalDialogAtom, subExperimentsMetaAtom } from "../../store/atoms";
import FormField from "../FormField/FormField";
import "../../styles/main.scss";
import "../DialogLayout/CreateDialog.scss";
import { CreateDialogFooter, FormState } from "../DialogLayout/CreateDialogFooter";
import { RenameSubexperiment } from "./types";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import Api from "../../api/api";

interface RenameSubexperimentDialogProps {
  subId: number;
  name: string;
}

export function RenameSubexperimentDialog({
  subId,
  name,
}: RenameSubexperimentDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const [formState, setFormState] = useState<FormState>();

  const handleRename = useRecoilCallback(
    ({ set }) => async (newName: string) => {
      const newSub = await Api.renameSubexperiment(subId, newName);
      set(subExperimentsMetaAtom(subId), newSub);
    },
    [subExperimentsMetaAtom]
  );

  return (
    <DialogLayout<RenameSubexperiment>
      dialogClass="primaryDialogWindow"
      size="medium"
      title="Rename subexperiment"
      description={
        <>
          Input new name for <b>{name}</b> sub-experiment
        </>
      }
      onClose={() => setModalDialog(undefined)}
      footer={<CreateDialogFooter submitBtnText="Rename" formState={formState} cancelBtn />}
      formik={{
        initialValues: { name },

        onSubmit: async form => {
          setFormState({ kind: "submitting" });
          try {
            await handleRename(form.name);
            setModalDialog(undefined);
          } catch (ex) {
            setFormState({ kind: "error", errorMsg: ex.message });
          }
        },
      }}
    >
      <FormField<string> name="name" label="Name">
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
