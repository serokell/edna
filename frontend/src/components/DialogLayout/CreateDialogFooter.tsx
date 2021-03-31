import React from "react";
import "./CreateDialog.scss";
import { useSetRecoilState } from "recoil";
import { Button } from "../Button/Button";
import { modalDialogAtom } from "../../store/atoms";

export type FormState = undefined | { kind: "submitting" } | { kind: "error"; errorMsg: string };

interface CreateDialogFooterProps {
  formState: FormState;
  editing?: boolean;
  cancelBtn?: boolean;
}

export function CreateDialogFooter({
  formState,
  cancelBtn,
  editing,
}: CreateDialogFooterProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  return (
    <>
      {formState?.kind === "error" && (
        <div className="createDialog__errorMsg">{formState.errorMsg}</div>
      )}

      {cancelBtn && (
        <Button
          disabled={formState?.kind === "submitting"}
          type="text"
          className="createDialog__footerBtn"
          onClick={() => {
            setModalDialog(undefined);
          }}
        >
          Cancel
        </Button>
      )}

      <Button
        loading={formState?.kind === "submitting"}
        disabled={formState?.kind === "submitting"}
        type="submit"
        className="createDialog__footerBtn"
      >
        {editing ? "Save" : "Create"}
      </Button>
    </>
  );
}
