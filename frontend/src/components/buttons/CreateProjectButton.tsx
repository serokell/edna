// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { useSetRecoilState } from "recoil";
import { Button } from "../Button/Button";
import PlusSvg from "../../assets/svg/plus.svg";
import { modalDialogAtom } from "../../store/atoms";

interface CreateProjectButtonProps {
  className?: string;
}

export function CreateProjectButton({ className }: CreateProjectButtonProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  return (
    <Button
      type="rounded"
      className={className}
      onClick={() => setModalDialog({ kind: "create-edit-project" })}
    >
      <PlusSvg />
      project
    </Button>
  );
}
