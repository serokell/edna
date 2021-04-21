import React from "react";
import { useSetRecoilState } from "recoil";
import { SubExperimentDto } from "../../api/types";
import { modalDialogAtom } from "../../store/atoms";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import { Button } from "../Button/Button";
import Api from "../../api/api";
import { useFilteredExperimentsRefresher } from "../../store/updaters";

interface DeleteSubexperimentDialogProps {
  subexperiment: SubExperimentDto;
}

export function DeleteSubexperimentDialog({
  subexperiment,
}: DeleteSubexperimentDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const refreshFiltered = useFilteredExperimentsRefresher();

  return (
    <DialogLayout
      dialogClass="secondaryDialogWindow"
      size="small"
      title="Delete subexperiment"
      onClose={() => setModalDialog(undefined)}
      description={
        <>
          Are you sure you want to delete the <b>{subexperiment.item.name}</b> subexperiment? This
          action cannot be undone
        </>
      }
      footer={
        <>
          <Button
            type="text"
            btnStyle="gray"
            onClick={() => {
              setModalDialog(undefined);
            }}
          >
            Cancel
          </Button>

          <Button type="text" isSubmit btnStyle="delete">
            Delete
          </Button>
        </>
      }
      formik={{
        initialValues: {},
        onSubmit: async () => {
          try {
            await Api.deleteSubexperiment(subexperiment.id);
            refreshFiltered();
            setModalDialog(undefined);
          } catch (ex) {
            console.log("Error on subexperiment delete", ex.message);
          }
        },
      }}
    />
  );
}