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
          control: provided => ({
            ...provided,
            border: "1px solid var(--color-gray)",
            borderRadius: "var(--border-radius)",
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
