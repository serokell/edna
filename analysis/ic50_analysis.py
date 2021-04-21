import io
import json
import sys
from typing import Any

import numpy as np

from ic50 import calculate_sigmoid, detect_outliers
from input_analysis import check_data, find_outliers_field


def stderr_print(*args, **kwargs):  # type: ignore
    print(*args, file=sys.stderr, **kwargs)


def main(inp: io.IOBase) -> None:
    data = json.load(inp)

    response = []

    for experiment in data:
        response_value = {"experiment": experiment["experiment"], "status": "DONE"}
        measurements = np.array(experiment['data'])

        find_outliers = find_outliers_field(experiment)

        input_check, input_error = check_data(measurements)
        if not input_check:
            response_value["status"] = input_error
            response.append(response_value)
            continue

        try:
            params = calculate_sigmoid(measurements)
            response_value["params"] = params
        except (ArithmeticError, RuntimeError) as exc:
            stderr_print(exc)
            response_value["status"] = "ERROR: Can not fit sigmoid."
            response.append(response_value)
            continue

        if find_outliers:
            try:
                outliers = detect_outliers(measurements)
                response_value["outliers"] = outliers
                if len(outliers) > 0:
                    new_measurements = np.delete(measurements, outliers, axis=0)
                    new_params = calculate_sigmoid(new_measurements)
                    response_value["new_params"] = new_params
            except ArithmeticError as exc:
                stderr_print(exc)
                response_value["outliers"] = []
                response.append(response_value)
                continue

        response.append(response_value)

    print(json.dumps(response))


if __name__ == '__main__':
    main(sys.stdin)
