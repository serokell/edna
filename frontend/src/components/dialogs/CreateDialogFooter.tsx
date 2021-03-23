import React from "react";
import "./CreateDialog.scss";
import { Button } from "../Button/Button";

export type FormState = undefined | { kind: "submitting" } | { kind: "error"; errorMsg: string };

interface CreateDialogFooterProps {
  formState: FormState;
}

export function CreateDialogFooter({ formState }: CreateDialogFooterProps): React.ReactElement {
  return (
    <div className="createDialog__footer">
      <Button
        loading={formState?.kind === "submitting"}
        disabled={formState?.kind === "submitting"}
        type="submit"
        className="createDialog__submitBtn"
      >
        Create
      </Button>
      {formState?.kind === "error" && (
        <div className="createDialog__errorMsg">{formState.errorMsg}</div>
      )}
    </div>
  );
}
