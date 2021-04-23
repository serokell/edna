// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { useSetRecoilState } from "recoil";
import { MethodologyDto } from "../../api/types";
import { modalDialogAtom } from "../../store/atoms";
import { DialogLayout } from "../DialogLayout/DialogLayout";
import { Button } from "../Button/Button";

interface MethodologyDescriptionDialogProps {
  methodology: MethodologyDto;
}

export function MethodologyDescriptionDialog({
  methodology,
}: MethodologyDescriptionDialogProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  return (
    <DialogLayout
      dialogClass="secondaryDialogWindow"
      size="large"
      title={methodology.item.name}
      onClose={() => setModalDialog(undefined)}
      description={methodology.item.description}
      footer={
        <Button
          type="text"
          btnStyle="gray"
          onClick={() => {
            setModalDialog(undefined);
          }}
        >
          Close
        </Button>
      }
    >
      {methodology.item.confluence && (
        <a href={methodology.item.confluence}>{methodology.item.confluence}</a>
      )}
    </DialogLayout>
  );
}
