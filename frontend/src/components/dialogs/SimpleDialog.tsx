// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { useSetRecoilState } from "recoil";
import { v4 as uuidv4 } from "uuid";
import { modalDialogAtom } from "../../store/atoms";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import { Button } from "../Button/Button";

interface SimpleDialogProps {
  title: string;
  description?: string | string[];
  btnText?: string;
}

export function SimpleDialog({
  title,
  description,
  btnText,
}: SimpleDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  return (
    <DialogLayout
      dialogClass="secondaryDialogWindow"
      size="large"
      title={title}
      onClose={() => setModalDialog(undefined)}
      description={
        typeof description === "string"
          ? description
          : description?.map(l => <p key={uuidv4()}>{l}</p>)
      }
      footer={
        <Button
          type="text"
          btnStyle="gray"
          onClick={() => {
            setModalDialog(undefined);
          }}
        >
          {btnText || "Ok"}
        </Button>
      }
    />
  );
}
