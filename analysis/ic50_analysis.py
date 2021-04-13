import json
import sys
from typing import Any

from ic50 import *


def main(argv: Any) -> None:
    data = json.loads(argv[0])

    response = []

    for experiment in data:
        response_value = {"experiment": experiment["experiment"], "status": "DONE"}
        measurements = np.array(experiment['data'])

        try:
            find_outliers = experiment["find_outliers"]
            if type(find_outliers) == str:
                find_outliers = find_outliers.lower()
                find_outliers = int(find_outliers == 'true')
        except KeyError:
            find_outliers = True

        if measurements.shape[1] != 2:
            response_value["status"] = "ERROR: Wrong experiment format."
            response.append(response_value)
            continue

        if np.unique(measurements[:, 0]).shape[0] < 4:
            response_value["status"] = "ERROR: Too little data."
            response.append(response_value)
            continue

        try:
            params = calculate_sigmoid(measurements)
            response_value["params"] = params
        except (ArithmeticError, RuntimeError):
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
            except ArithmeticError:
                response_value["outliers"] = []
                response.append(response_value)
                continue

        response.append(response_value)

    print(json.dumps(response))


if __name__ == '__main__':
    main(sys.argv[1:])
