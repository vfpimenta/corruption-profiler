#!/usr/bin/python3

from sklearn.metrics import pairwise
from optparse import OptionParser
from profiler import Profiler
import pandas as pd
import json

parser = OptionParser()
parser.add_option('-s', '--series-type', dest='series_type', type='str',
    help='Input series [flight|publicity|telecom]', metavar='STRING')

(options, args) = parser.parse_args()

def main():
    series_type = options.series_type
    if not series_type:
      series_type = 'default'

    if series_type == 'default':
      subquota_description = None
    elif series_type == 'flight':
      subquota_description = 'Flight ticket issue'
    elif series_type == 'publicity':
      subquota_description = 'Publicity of parliamentary activity'
    elif series_type == 'telecom':
      subquota_description = 'Telecommunication'
    elif series_type == 'fuels':
      subquota_description = 'Fuels and lubricants'

    profiler = Profiler(light=True)
    series = profiler.read_congressman_json(54, subquota_description=subquota_description)
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