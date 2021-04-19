import warnings
from typing import Dict, List, Tuple, Union

import numpy as np
from scipy.optimize import OptimizeWarning, curve_fit
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import MinMaxScaler


def _sigmoid(x: Union[np.array, float], a: float, b: float, c: float, d: float) -> Union[np.array, float]:
    return ((a - d) / (1.0 + ((x / c) ** b))) + d


def _get_starting_parameters(concentrations: np.array, scaled_measurements: np.array) -> List[float]:
    a = scaled_measurements[0]
    d = scaled_measurements[-1]
    b = 1
    c = concentrations[(np.abs(scaled_measurements - (a + d) / 2)).argmin()]
    return [a, b, c, d]


def _fit_sigmoid(
        measurements: np.array,
        scaler: MinMaxScaler, fit_scaler: bool = True,
) -> Tuple[np.array, np.array, float, MinMaxScaler]:
    if fit_scaler:
        scaled_signal = scaler.fit_transform(measurements[:, 1].reshape(-1, 1)).reshape(-1)
    else:
        scaled_signal = scaler.transform(measurements[:, 1].reshape(-1, 1)).reshape(-1)

    starting_point = _get_starting_parameters(measurements[:, 0], scaled_signal)

    warnings.simplefilter('ignore', OptimizeWarning)
    try:
        with np.errstate(invalid='ignore'):
            fit_param = curve_fit(_sigmoid, measurements[:, 0], np.array(scaled_signal),
                                  p0=starting_point)
    except RuntimeError:
        with np.errstate(invalid='ignore'):
            fit_param = curve_fit(_sigmoid, measurements[:, 0], np.array(scaled_signal),
                                  p0=starting_point, maxfev=10000)

    fitted_values = _sigmoid(measurements[:, 0], *fit_param[0])
    mse = mean_squared_error(scaled_signal, fitted_values)

    return fitted_values, fit_param[0], mse, scaler


def _pseudo_cook_distance(
        global_fit: np.array,
        stripped_fit: np.array,
        global_mse: float,
        num_parameters: int = 4,
) -> float:
    cook_distance = float(((global_fit - stripped_fit) ** 2).sum()) / ((num_parameters + 1) * global_mse + 1e-10)
    return cook_distance


def _calc_cut_off_value(num_measurements: int, num_parameters: int = 4, threshold: int = 3) -> float:
    return threshold * (num_parameters + 1) / num_measurements


def _calc_cook_distances(measurements: np.array, total_fitted_values: np.array,
                         total_mse: float, scaler: MinMaxScaler) -> Dict[int, float]:
    cook_distances = {}
    for idx in range(measurements.shape[0]):
        stripped_data = np.delete(measurements, idx, axis=0)
        stripped_values, _, _, _ = _fit_sigmoid(stripped_data, scaler, fit_scaler=False)
        cook_distances[idx] = _pseudo_cook_distance(np.delete(total_fitted_values, idx),
                                                    stripped_values, total_mse)
    return cook_distances


def detect_outliers(measurements: np.array) -> List[int]:
    sorted_indices = np.lexsort((measurements[:, 0], measurements[:, 1]))
    measurements = measurements[sorted_indices]
    scaler = MinMaxScaler()
    total_fitted_values, _, total_mse, scaler = _fit_sigmoid(measurements, scaler)
    cook_distances = _calc_cook_distances(measurements, total_fitted_values, total_mse, scaler)
    outliers = []
    cut_off_value = _calc_cut_off_value(len(cook_distances))
    for key, value in cook_distances.items():
        if value >= cut_off_value:
            outliers.append(int(sorted_indices[key]))
    return outliers


def calculate_sigmoid(measurements: np.array) -> List[int]:
    measurements = measurements[np.lexsort((measurements[:, 0], measurements[:, 1]))]
    scaler = MinMaxScaler()
    total_fitted_values, sigmoid_params, total_mse, scaler = _fit_sigmoid(measurements, scaler)
    transformed = scaler.inverse_transform(np.array([sigmoid_params[0], sigmoid_params[3]]).reshape(-1, 1)).reshape(-1)
    return [transformed[0], sigmoid_params[1], sigmoid_params[2], transformed[1]]
