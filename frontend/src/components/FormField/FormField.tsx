import React from "react";
import "./FormField.scss";
import cn from "classnames";
import { ErrorMessage, Field, useField } from "formik";

interface FormFieldProps<V> {
  name: string;
  label: string;
  className?: string;
  children?: (field: FormikCompatible<V>) => React.ReactNode | React.ComponentType;

  [prop: string]: any;
}

export interface FormikCompatible<T> {
  value: T;
  onChange: (newValue: T) => void;
}

function FormField<V>({ name, label, children, className, ...props }: FormFieldProps<V>) {
  // eslint-disable-next-line no-empty-pattern
  const [{}, { value }, { setValue }] = useField<V>(name);
  return (
    <div className={cn("formField", children && className)}>
      <div className="formField__label">
        {label}
        <span className="formField__error">
          <ErrorMessage name={name} />
        </span>
      </div>
      <Field name={name} {...props} className={!children && className}>
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
