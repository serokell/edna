import React from "react";
import "./CreateDialog.scss";
import { useSetRecoilState } from "recoil";
import { Button } from "../Button/Button";
import { modalDialogAtom } from "../../store/atoms";

export type FormState = undefined | { kind: "submitting" } | { kind: "error"; errorMsg: string };

interface CreateDialogFooterProps {
  formState: FormState;
  editing?: boolean;
}

export function CreateDialogFooter({
  formState,
  editing,
}: CreateDialogFooterProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  return (
    <div className="createDialog__footer">
      {formState?.kind === "error" && (
        <div className="createDialog__errorMsg">{formState.errorMsg}</div>
      )}

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

      <Button
        loading={formState?.kind === "submitting"}
        disabled={formState?.kind === "submitting"}
        type="submit"
        className="createDialog__footerBtn"
      >
        {editing ? "Save" : "Create"}
      </Button>
    </div>
  );
}
