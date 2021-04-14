import React from "react";
import { useSetRecoilState } from "recoil";
import { modalDialogAtom } from "../../store/atoms";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import { Button } from "../Button/Button";

interface SimpleDialogProps {
  title: string;
  description?: string;
}

export function SimpleDialog({ title, description }: SimpleDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  return (
    <DialogLayout
      dialogClass="secondaryDialogWindow"
      size="large"
      title={title}
      onClose={() => setModalDialog(undefined)}
      description={description}
      footer={
        <Button
          type="text"
          btnStyle="gray"
          onClick={() => {
            setModalDialog(undefined);
          }}
        >
          Ok
        </Button>
      }
    >
      {description}
    </DialogLayout>
  );
}
