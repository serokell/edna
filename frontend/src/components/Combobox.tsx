import React from "react";
import Select from "react-select";
import { Loadable } from "recoil";
import { GroupTypeBase } from "react-select/src/types";
import { StylesConfig } from "react-select/src/styles";
import { Maybe } from "../utils/utils";
import { FormikCompatible } from "./FormField/FormField";

export type SelectOption = { value: string; label: string };

interface ComboboxProps<T> extends FormikCompatible<Maybe<T>> {
  optionsLoadable: Loadable<T[]>;
  placeholder: string;
  placeholderEmpty: string;
  toOption: (arg: T) => SelectOption;
  isLoading?: boolean;
  styles?: StylesConfig<SelectOption, false, GroupTypeBase<SelectOption>>;

  [prop: string]: any;
}

export default function Combobox<T>({
  optionsLoadable,
  placeholder,
  placeholderEmpty,
  toOption,
  optionCreator,
  value,
  onChange,
  styles,
  isLoading,
  ...props
}: ComboboxProps<T>): React.ReactElement {
  const mergedStyles: StylesConfig<SelectOption, false, GroupTypeBase<SelectOption>> = {
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
    ...styles,
  };
  return (
    <Select<SelectOption>
      {...props}
      value={value ? toOption(value) : null}
      onChange={x => {
        const opt =
          optionsLoadable.state === "hasValue" && optionsLoadable.contents
            ? optionsLoadable.contents.find(o => toOption(o).value === x?.value)
            : undefined;
        onChange(opt);
      }}
      isLoading={isLoading || optionsLoadable.state === "loading"}
      isClearable
      styles={mergedStyles}
      // Show error happened during options loading
      noOptionsMessage={({ inputValue }) => {
        if (!inputValue)
          return optionsLoadable.state === "hasError"
            ? "Error happened during loading"
            : placeholderEmpty;
        return placeholderEmpty;
      }}
      placeholder={placeholder}
      options={
        (optionsLoadable.state === "hasValue" && optionsLoadable.contents.map(toOption)) ||
        undefined
      }
    />
  );
}
