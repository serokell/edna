import io
import json
import os

import pandas as pd
import pytest

from ic50 import *
from ic50_analysis import main as ic50_calc


def ic50_calc_str(arg: str):
    return ic50_calc(io.StringIO(arg))


@pytest.fixture
def load_sample_json():  # type: ignore
    with open('tests/sample_json.json') as fp:
        data = json.load(fp)
    return data


@pytest.fixture
def load_sample_data():  # type: ignore
    sample_data = []
    exp_num = 0
    for file_name in os.listdir('tests/sample_data'):
        csv_file = pd.read_csv('tests/sample_data/' + file_name)
        json_file = {'experiment': exp_num, 'data': list(map(list, np.array(csv_file)))}
        exp_num += 1
        sample_data.append(json_file)
    return json.dumps(sample_data)


def test_no_outliers(load_sample_json):  # type: ignore
    measurements = np.array(load_sample_json['sane']['data'])

    outliers = detect_outliers(measurements)

    assert len(outliers) == 0


def test_one_outlier(load_sample_json):  # type: ignore
    measurements = np.array(load_sample_json['insane']['data'])

    outliers = detect_outliers(measurements)

    assert len(outliers) == 1


# noinspection PyBroadException
def test_no_exceptions(load_sample_json):
    exp = [load_sample_json['sane']]
    try:
        ic50_calc_str(json.dumps(exp))
    except Exception:
        assert False


# noinspection PyBroadException
def test_shuffle_points_sigmoid(load_sample_json):
    exp_1 = load_sample_json['sane']
    exp_2 = load_sample_json['sane_shuffled']
    params_1 = calculate_sigmoid(np.array(exp_1['data']))
    params_2 = calculate_sigmoid(np.array(exp_2['data']))
    assert params_1 == params_2


# noinspection PyBroadException
def test_shuffle_points_outliers(load_sample_json):
    exp_1 = load_sample_json['insane']
    exp_2 = load_sample_json['insane_shuffled']
    outliers_1 = detect_outliers(np.array(exp_1['data']))
    outliers_2 = detect_outliers(np.array(exp_2['data']))
    assert outliers_1 != outliers_2


# noinspection PyBroadException
def test_single_concentration(load_sample_json):  # type: ignore
    exp = [load_sample_json['single_concentration']]
    try:
        ic50_calc_str(json.dumps(exp))
    except Exception:
        assert False


# noinspection PyBroadException
def test_little_data(load_sample_json):  # type: ignore
    exp = [load_sample_json['little_data']]
    try:
        ic50_calc_str(json.dumps(exp))
    except Exception:
        assert False


# noinspection PyBroadException
def test_sample_data_files(load_sample_data):
    try:
        ic50_calc_str(load_sample_data)
    except Exception:
        assert False
