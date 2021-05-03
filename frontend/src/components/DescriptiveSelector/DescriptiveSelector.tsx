// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Loadable } from "recoil";
import cx from "classnames";
import React, { useState } from "react";
import { isDefined, Maybe } from "../../utils/utils";
import Combobox, { SelectOption } from "../Combobox";
import "./DescriptiveSelector.scss";
import { DescriptivePlate, EntityProperty } from "./DescriptivePlate";

interface DescriptiveSelectorProps<T> {
  value: Maybe<T>;
  onChange: (newValue: Maybe<T>) => void;
  optionsLoadable: Loadable<T[]>;
  placeholder: string;
  placeholderEmpty: string;
  toOption: (arg: T) => SelectOption;
  toEntityProperties?: (val: T) => EntityProperty[];
  isLoading?: boolean;
  className?: string;
  contextActions?: React.ReactNode[];
  optionsFilter?: (val: T[]) => T[] | undefined;
}

export function DescriptiveSelector<T>({
  value,
  toEntityProperties,
  className,
  contextActions,
  ...props
}: DescriptiveSelectorProps<T>): React.ReactElement {
  const [selectOpened, setSelectOpened] = useState(false);
  const hasPlate = !selectOpened && isDefined(value) && isDefined(toEntityProperties);
  const borderStyles = hasPlate
    ? {
        borderBottomLeftRadius: 0,
        borderBottomRightRadius: 0,
      }
    : {};

  return (
    <div className={cx("descriptiveSelector", className)}>
      <Combobox
        {...props}
        isFocused={selectOpened}
        styles={{
          control: (provided: any, state) => ({
            ...provided,
            border: "1px solid var(--color-gray)",
            borderRadius: "var(--border-radius)",
            borderColor: "hsl(0, 0%, 80%)",
            boxShadow: state.isFocused ? 0 : 0,
            "&:hover": {
              borderColor: "hsl(0, 0%, 80%)",
            },
            ...borderStyles,
          }),
        }}
        value={value}
        onMenuOpen={() => setSelectOpened(true)}
        onMenuClose={() => setSelectOpened(false)}
      />
      {!selectOpened && isDefined(value) && isDefined(toEntityProperties) && (
        <DescriptivePlate properties={toEntityProperties(value)} contextActions={contextActions} />
      )}
    </div>
  );
}
