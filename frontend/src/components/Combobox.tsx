// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import Select from "react-select";
import { Loadable } from "recoil";
import { GroupTypeBase } from "react-select/src/types";
import { StylesConfig } from "react-select/src/styles";
import { Maybe } from "../utils/utils";
import { FormikCompatible } from "./FormField/FormField";

export type SelectOption = { value: string; label: string; isDisabled?: boolean };

interface ComboboxProps<T> extends FormikCompatible<Maybe<T>> {
  optionsLoadable: Loadable<T[]>;
  placeholder: string;
  placeholderEmpty: string;
  toOption: (arg: T) => SelectOption;
  isLoading?: boolean;
  styles?: StylesConfig<SelectOption, false, GroupTypeBase<SelectOption>>;
  isDisabled?: boolean;
  optionsFilter?: (val: T[]) => T[] | undefined;
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
  isDisabled = false,
  optionsFilter,
  ...props
}: ComboboxProps<T>): React.ReactElement {
  const mergedStyles = {
    // Colorize message when error happened
    noOptionsMessage: (provided: any) =>
      optionsLoadable.state === "hasError"
        ? {
            ...provided,
            color: "var(--error-msg)",
          }
        : { ...provided },
    // Show loading indicator on loading
    loadingIndicator: (provided: any) =>
      optionsLoadable.state === "loading" ? { visibility: "hidden" } : { ...provided },
    option: (provided: any, state: any) => ({
      ...provided,
      backgroundColor: state.isSelected
        ? "#bfe5d2"
        : state.isFocused
        ? "#edf8f2"
        : state.isDisabled
        ? "#f0f0f0"
        : "white",
      color: state.isDisabled ? "rgba(25, 28, 28, 0.5)" : "#515151",
      "&:active": {
        backgroundColor: state.isDisabled ? "#f0f0f0" : "#bfe5d2",
      },
      height: state.isDisabled ? "34px" : "40px",
      padding: state.isDisabled ? "8px 22px" : "11px 22px",
    }),
    control: (provided: any, state: any) => ({
      ...provided,
      borderColor: "hsl(0, 0%, 80%)",
      // This line disable the blue border
      boxShadow: state.isFocused ? 0 : 0,
      "&:hover": {
        borderColor: "hsl(0, 0%, 80%)",
      },
    }),
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
        (optionsLoadable.state === "hasValue" &&
          (optionsFilter
            ? optionsFilter(optionsLoadable.contents)?.map(toOption)
            : optionsLoadable.contents.map(toOption))) ||
        undefined
      }
      isDisabled={isDisabled}
    />
  );
}
