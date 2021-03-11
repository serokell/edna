import React from "react";
import "./FormField.scss";
import cn from "classnames";
import { ErrorMessage, Field, useField } from "formik";

interface FormFieldProps<V> {
  name: string;
  label: string;
  className?: string;
  classNameInner?: string;
  children?: (field: FormikCompatible<V>) => React.ReactNode | React.ComponentType;

  [prop: string]: any;
}

export interface FormikCompatible<T> {
  value: T;
  onChange: (newValue: T) => void;
}

function FormField<V>({
  name,
  label,
  children,
  className,
  classNameInner,
  ...props
}: FormFieldProps<V>): React.ReactElement {
  // eslint-disable-next-line no-empty-pattern
  const [{}, { value }, { setValue }] = useField<V>(name);
  return (
    <div className={cn("formField", className)}>
      <div className="formField__label">
        {label}
        <span className="formField__error">
          <ErrorMessage name={name} />
        </span>
      </div>
      <Field {...props} name={name} className={classNameInner}>
        {children &&
          (() =>
            children({
              value,
              onChange: (x: V) => setValue(x),
            }))}
      </Field>
    </div>
  );
}

export default FormField;
