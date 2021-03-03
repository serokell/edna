import React, { useState } from "react";
import Select from "react-select/creatable";
import { Loadable } from "recoil";
import { useField } from "formik";
import { isDefined, Maybe } from "../utils/utils";

type SelectOption = { value: string; label: string };

interface CreatableSelectProps<T> {
  optionsLoadable: Loadable<T[]>;
  placeholder: string;
  placeholderNo: string;
  toOption: (arg: T) => SelectOption;
  optionCreator: (value: string) => Promise<Maybe<T>>;

  name: string;
  [prop: string]: any;
}

export default function CreatableSelect<T>({
  optionsLoadable,
  placeholder,
  placeholderNo,
  toOption,
  optionCreator,
  name,
}: CreatableSelectProps<T>) {
  const [optionCreating, setOptionCreating] = useState(false);
  const [curOpt, setCurOpt] = useState<SelectOption | null>(null);
  const { setValue } = useField(name)[2];

  return (
    <Select
      value={curOpt}
      onChange={newVal => {
        setCurOpt(newVal);
        setValue(newVal);
      }}
      isDisabled={optionCreating}
      isLoading={optionsLoadable.state === "loading" || optionCreating}
      isClearable
      styles={{
        // Colorize message when error happened
        noOptionsMessage: provided =>
          optionsLoadable.state === "hasError"
            ? {
                ...provided,
                color: "var(--error-msg)",
              }
            : { ...provided },
        // Show loading indicator on loading
        loadingIndicator: provided =>
          optionsLoadable.state === "loading" ? { visibility: "hidden" } : { ...provided },
      }}
      // Show error happened during options loading
      noOptionsMessage={({ inputValue }) => {
        if (!inputValue)
          return optionsLoadable.state === "hasError"
            ? "Error happened during loading"
            : placeholderNo;
        return placeholderNo;
      }}
      placeholder={placeholder}
      options={
        (optionsLoadable.state === "hasValue" && optionsLoadable.contents.map(toOption)) ||
        undefined
      }
      onCreateOption={async optionName => {
        try {
          setOptionCreating(true);
          const newOpt = await optionCreator(optionName);
          if (isDefined(newOpt)) {
            setCurOpt(toOption(newOpt));
          }
        } finally {
          setOptionCreating(false);
        }
      }}
    />
  );
}
