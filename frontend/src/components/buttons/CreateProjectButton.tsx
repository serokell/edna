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
    <Button type="secondary" className={className} onClick={() => setModalDialog("create-project")}>
      <PlusSvg /> project
    </Button>
  );
}