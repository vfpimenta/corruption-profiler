#!/usr/bin/python3

from sklearn.neural_network import MLPClassifier
from sklearn.ensemble import RandomForestClassifier
from optparse import OptionParser
from profiler import Profiler
from itertools import cycle
import numpy as np
import json
import warnings
import random
warnings.filterwarnings("ignore")

parser = OptionParser()
parser.add_option('-s', '--series-type', dest='series_type', type='str',
    help='Input series [flight|publicity|telecom|fuels]', metavar='STRING')

(options, args) = parser.parse_args()

def read_mstknn_dump(legislature, series_type, k, method='JS'):
  with open('../data/{}/dump/{}/k-{}/dump-clusters-{}.json'.format(series_type, method, k, legislature)) as jsonfile:    
    data = json.load(jsonfile)

  return data

def split_set(clusters, data, rate=(3,1)):
  train_set = list()
  test_set = list()

  num_move = len(data.keys()) * rate[0] / (rate[0] + rate[1])
  pool = cycle(range(len(clusters)))
  buffer_list = list()

  for label in pool:
    to_move = random.choice(clusters[label])
    if to_move not in buffer_list:
      train_set.append((to_move, data[to_move][4]))
      buffer_list.append(to_move)

    if len(buffer_list) >= num_move:
      break

  for remainder in data.keys():
    if remainder not in buffer_list:
      test_set.append((remainder, data[remainder][4]))

  return train_set, test_set

def split_labels(label_list, train_set, test_set):
  train_labels = list()
  test_labels = list()
  label_dict = dict()

  cluster_idx = 0
  for cluster in label_list:
    for label in cluster:
      label_dict[label] = cluster_idx
    cluster_idx += 1

  for sample in train_set:
    train_labels.append(label_dict[sample[0]])

  for sample in test_set:
    test_labels.append(label_dict[sample[0]])

  return train_labels, test_labels

def main():
  legislature = 54
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

  print("Reading base data...")
  profiler = Profiler(light=True)
  series = profiler.read_congressman_json(legislature=legislature, subquota_description=subquota_description)
  # ===========================================================================
  with open('../data/JSON/congressman_{}_outliers.json'.format(legislature)) as jsonfile:    
      file_outliers = json.load(jsonfile)
  valid_series = dict()
  for congressman_id in series.keys():
      congressman = series.get(congressman_id)
      if congressman_id not in file_outliers:
          valid_series[congressman_id] = congressman
  # ===========================================================================
  print("Reading base data... Done")

  for k in [2, 3, 4, 5]:
    for method in ["robust", "JS", "cosine"]:
      print("Running classifier for series_type={}, k={}, method={}".format(series_type, k, method))

      clusters = read_mstknn_dump(legislature, series_type, k, method)
      train_set, test_set = split_set(clusters, valid_series)
      train_labels, test_labels = split_labels(clusters, train_set, test_set)

      classifier = RandomForestClassifier(oob_score=True)
      classifier.fit([t[1] for t in train_set], train_labels)

      hits = [0] * len(clusters)
      for i in range(len(clusters)):
        for sample in zip(test_set, test_labels):
          if sample[1] == i:
            predicted_label = classifier.predict(sample[0][1])
            if predicted_label == sample[1]:
              hits[i] += 1
        hits[i] = hits[i] / test_labels.count(i) if test_labels.count(i) > 0 else 0.0

      for i in range(len(hits)):
        hitrate = hits[i]
        print("Cluster {}, Accuracy: {}".format(i, hitrate))
      print("Average: {}".format(np.mean(hits)))
      print("==============================================================")

if __name__ == '__main__':
  main()