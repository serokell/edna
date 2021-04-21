# Edna analysis

This folder contains implementation of Edna data analysis in Python.

## Usage

First, normally you don't want to call this code directly, it is supposed to be called from `edna-server`.
We use [`poetry`](https://python-poetry.org/) for dependency management, though you can install dependencies using your
favorite way and not use `poetry`.
To get shell with all dependencies installed run `poetry install` and then `poetry shell` from this directory.

## Features

The list of features implemented in an analytic module:
1. Compute parameters of fitted sigmoid.
2. Detect potential outliers.
3. Compute parameters of fitted sigmoid without outliers.

### Input format

JSON containing list of experiments in the form:
```
[{"experiment": int,
"data": [[float, float], ...],
"find_outliers": boolean},
...]
```

It's expected to be passed to the standard input (`stdin`).

### Output format

JSON containing list of calculated values per each experiment:
```
[{"experiment": int,
"params": [float, float, float, float],
"outliers": [int, ...],
"new_params": [float, float, float, float],
"status": str},
...]
```
`experiment` and `status` are always present in the output.
If `"status"` is not `DONE`, other fields are omitted. If `find_outliers` is `False`, outliers are not calculated.
If there are no outliers, `new_params` is omitted. Status may contain error descriptions.

## Error description

1. `ERROR: Empty data field.` No data points have been passed.
2. `ERROR: Wrong experiment format.` The experiment from .json provided is in the wrong form.
3. `ERROR: Too little data.` The least amount of data points to fit sigmoid is 4.
4. `ERROR: Can not fit sigmoid.` Impossible to fit sigmoid to this particular set of data points.
