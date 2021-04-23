# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

from typing import Dict, Tuple, Union

import numpy as np


def _check_empty_data(measurements: np.array) -> bool:
    if measurements.shape[0] == 0:
        return True
    else:
        return False


def _check_not_2d(measurements: np.array) -> bool:
    if (len(measurements.shape) != 2) or (measurements.shape[1] != 2):
        return True
    else:
        return False


def _check_not_enough_data(measurements: np.array) -> bool:
    if np.unique(measurements[:, 0]).shape[0] < 4:
        return True
    else:
        return False


def _check_nan(measurements: np.array) -> bool:
    if np.isnan(measurements).sum() > 0:
        return True
    else:
        return False


def check_data(measurements: np.array) -> Tuple[bool, str]:
    if _check_empty_data(measurements):
        return False, "ERROR: Empty data field."
    if (_check_not_2d(measurements)) or (_check_nan(measurements)):
        return False, "ERROR: Wrong experiment format."
    if _check_not_enough_data(measurements):
        return False, "ERROR: Too little data."
    return True, ""


def find_outliers_field(experiment: Dict) -> Union[int, bool]:
    find_outliers = experiment.get("find_outliers", True)
    if type(find_outliers) == str:
        find_outliers = find_outliers.lower()
        find_outliers = int(find_outliers == 'true')
    return find_outliers
