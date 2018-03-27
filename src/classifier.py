#!/usr/bin/python3

from sklearn.neural_network import MLPClassifier
from sklearn.ensemble import RandomForestClassifier
from profiler import Profiler
import numpy as np
import json
import warnings
warnings.filterwarnings("ignore")

def read_mstknn_dump(legislature, series_type, k, method='JS'):
  with open('../data/{}/dump/{}/k-{}/dump-clusters-{}.json'.format(series_type, method, k, legislature)) as jsonfile:    
    data = json.load(jsonfile)

  return data

def split_set(data, rate=(1,2)):
  train_set = list()
  test_set = list()

  counter = 0
  keys_list = list(data.keys())
  np.random.shuffle(keys_list)
  for key in keys_list:
    if counter <= len(data.keys()) * rate[0] / (rate[0] + rate[1]):
      train_set.append((key, data.get(key)[4]))
    else:
      test_set.append((key, data.get(key)[4]))
    counter += 1

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

  print("Reading base data...")
  profiler = Profiler()
  series = profiler.read_congressman_json(legislature=legislature)
  # ===========================================================================
  with open('../data/JSON/congressman_{}_outliers.json'.format(legislature)) as jsonfile:    
      file_outliers = json.load(jsonfile)
  valid_series = dict()
  for congressman_id in series.keys():
      congressman = series.get(congressman_id)
      if congressman_id not in file_outliers:
          valid_series[congressman_id] = congressman
  # ===========================================================================
  train_set, test_set = split_set(valid_series)
  print("Reading base data... Done")

  for series_type in ['fuels']:
    for k in [2, 3, 4, 5]:
      for method in ["robust", "JS", "cosine"]:
        print("Running classifier for series_type={}, k={}, method={}".format(series_type, k, method))

        clusters = read_mstknn_dump(legislature, series_type, k, method)
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
          hits[i] = hits[i] / test_labels.count(i) if test_labels.count(i) > 0 else 0

        accuracy = np.mean(hits)
        print("Accuracy: {}".format(accuracy))
        print("==============================================================")

if __name__ == '__main__':
  main()