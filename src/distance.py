#!/usr/bin/python3

from sklearn.metrics import pairwise
from profiler import Profiler
import pandas as pd
import json

def main():
    profiler = Profiler(light=True)
    series = profiler.read_congressman_json(54)
    with open('../data/JSON/congressman_54_outliers.json') as jsonfile:
        file_outliers = json.load(jsonfile)
    series_list = list()
    keys_list = list()
    for congressman_id in series.keys():
        congressman = series.get(congressman_id)
        if congressman_id not in file_outliers:
            keys_list.append(congressman_id)
            series_list.append(congressman[-1])

    distances = pairwise.euclidean_distances(series_list, series_list)
    df = pd.DataFrame(distances)
    df.columns = keys_list
    df.index = keys_list

    df.to_csv("distance-kmeans.csv")

if __name__ == '__main__':
    main()