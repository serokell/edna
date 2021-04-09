import React from "react";
import { useSetRecoilState } from "recoil";
import { MethodologyDto } from "../../api/types";
import { modalDialogAtom } from "../../store/atoms";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import { Button } from "../Button/Button";
import Api from "../../api/api";
import { useMethodologiesRefresher } from "../../store/updaters";

interface DeleteMethodologyDialogProps {
  methodology: MethodologyDto;
}

export function DeleteMethodologyDialog({
  methodology,
}: DeleteMethodologyDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const refreshMethodologies = useMethodologiesRefresher();

  return (
    <DialogLayout
      dialogClass="secondaryDialogWindow"
      size="small"
      title={methodology.item.name}
      onClose={() => setModalDialog(undefined)}
      description={`Are you sure you want to delete the "${methodology.item.name}" methodology?
        This action cannot be undone`}
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
            await Api.deleteMethodology(methodology.id);
            setModalDialog(undefined);
            refreshMethodologies();
          } catch (ex) {
            console.log("Error on methodology delete", ex.message);
          }
        },
      }}
    />
  );
}
