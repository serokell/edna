import React, { useState } from "react";
import { useSetRecoilState } from "recoil";
import { modalDialogAtom } from "../../store/atoms";
import FormField from "../FormField/FormField";
import "../../styles/main.scss";
import "../DialogLayout/CreateDialog.scss";
import Api from "../../api/api";
import { CreateDialogFooter, FormState } from "../DialogLayout/CreateDialogFooter";
import { useCompoundsRefresher, useMethodologiesRefresher } from "../../store/updaters";
import { AddLinkForm } from "./types";
import { CompoundDto, MethodologyDto } from "../../api/types";
import { DialogLayout } from "../DialogLayout/DialogLayout";

interface AddLinkDialogAbstractProps {
  title: string;
  description: React.ReactNode;
  link?: string;
  onLinkSave: (newLink: string) => void;
}

function AddLinkDialogAbstract({
  title,
  link,
  description,
  onLinkSave,
}: AddLinkDialogAbstractProps): React.ReactElement {
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const [formState, setFormState] = useState<FormState>();

  return (
    <DialogLayout<AddLinkForm>
      dialogClass="primaryDialogWindow"
      size="medium"
      title={title}
      description={description}
      onClose={() => setModalDialog(undefined)}
      footer={
        <CreateDialogFooter submitBtnText="Save" formState={formState} editing={!!link} cancelBtn />
      }
      formik={{
        initialValues: { link: link ?? "" },

        onSubmit: async form => {
          setFormState({ kind: "submitting" });
          try {
            await onLinkSave(form.link);
            setModalDialog(undefined);
          } catch (ex) {
            setFormState({ kind: "error", errorMsg: ex.message });
          }
        },
      }}
    >
      <FormField<string> name="link" label="Link">
        {field => (
          <input
            className="ednaInput"
            value={field.value}
            onChange={e => field.onChange(e.target.value)}
          />
        )}
      </FormField>
    </DialogLayout>
  );
}

interface AddLinkDialogProps {
  target:
    | { kind: "methodology"; object: MethodologyDto }
    | { kind: "compound"; object: CompoundDto };
}

export function AddLinkDialog({ target }: AddLinkDialogProps): React.ReactElement {
  const { name } = target.object.item;
  const refreshMethodologies = useMethodologiesRefresher();
  const refreshCompounds = useCompoundsRefresher();
  const editing =
    (target.kind === "methodology" && target.object.item.confluence) ||
    (target.kind === "compound" && target.object.item.chemSoft);
  const editingLink =
    target.kind === "methodology" ? target.object.item.confluence : target.object.item.chemSoft;

  const title = `${editing ? "Edit" : "Add"} ${
    target.kind === "methodology" ? "confluence" : "ChemSoft"
  } link`;
  const descr = (
    <>
      {title} for <b>{name}</b> {target.kind}
    </>
  );
  return (
    <AddLinkDialogAbstract
      title={title}
      description={descr}
      link={editingLink}
      onLinkSave={async newLink => {
        if (target.kind === "compound") {
          await Api.updateChemSoftLink(target.object.id, newLink);
          refreshCompounds();
        } else {
          await Api.editMethodology(target.object.id, {
            name: target.object.item.name,
            description: target.object.item.description,
            confluence: newLink,
          });
          refreshMethodologies();
        }
      }}
    />
  );
}
