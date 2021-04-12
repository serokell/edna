import json
import sys

import numpy as np
from scipy.optimize import curve_fit
from sklearn.preprocessing import MinMaxScaler


def find_nearest_idx(array, value):
    array = np.asarray(array)
    idx = (np.abs(array - value)).argmin()
    return idx


def sigmoid(x, a, b, c, d):
    return ((a - d) / (1.0 + ((x / c) ** b))) + d


def replace_duplicates(measurements):
    unique_concentrations = [c for c in {s[0] for s in measurements} if not np.isnan(c)]

    unique_measurements = []
    for c in unique_concentrations:
        specific_measurements = measurements[measurements[:, 0] == c]
        specific_signals = [s[1] for s in specific_measurements]
        unique_measurements.append([c, np.mean(specific_signals)])

    unique_measurements = np.array(unique_measurements)
    sorted_measurements = unique_measurements[unique_measurements[:, 0].argsort()]
    return np.array([s[0] for s in sorted_measurements]), np.array(
        [s[1] for s in sorted_measurements])


def compute_sigmoid(measurements):
    concentrations, signals = replace_duplicates(measurements)

    scaler = MinMaxScaler()
    signals_scalered = scaler.fit_transform(signals.reshape(-1, 1))
    signals_scalered = signals_scalered.reshape(-1)

    res1, _ = curve_fit(sigmoid, concentrations, signals_scalered,
                        [signals_scalered[0], 1, concentrations[
                            find_nearest_idx(signals_scalered, (
                                    np.max(signals_scalered) - np.min(signals_scalered)) / 2)],
                         signals_scalered[-1]])

    transformed = scaler.inverse_transform(np.array([res1[0], res1[3]]).reshape(-1, 1)).reshape(-1)

    return [transformed[0], res1[1], res1[2], transformed[1]]


def main(argv):
    data = json.loads(argv[0])

    response = []

    for value in data:
        result = compute_sigmoid(np.array(value["data"], dtype=np.float32))
        response_value = {"experiment": value["experiment"], "data": {"Right": result}}
        response.append(response_value)

    print(json.dumps(response))


if __name__ == "__main__":
    main(sys.argv[1:])
