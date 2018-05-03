import json
import csv
import random

def merge_min_clusters(clusters, min_value, legislature):
  merged = list()
  min_cluster = list()
  for cluster in clusters:
    if isinstance(cluster,list):
      if len(cluster) <= min_value:
        min_cluster = min_cluster + cluster
      else:
        merged.append(cluster)
    elif isinstance(cluster, str):
      min_cluster.append(cluster)

  if len(min_cluster) > 0:
    merged.append(min_cluster)

  filepath = '__pycache__/term-{}-cluster-{}.out'.format(legislature,random.randint(10000,99999))
  with open(filepath, 'w') as file:
    file.write(str(merged))

  return merged

def stats(result, target, data):
  tp = 0
  fp = 0
  fn = len(target)
  tn = len(data) - len(result)

  for el in result:
    if el in target:
      tp += 1
      fn -= 1
    else:
      fp += 1

  return tp, fp, fn, tn

def precision(tp, fp, fn, tn):
  return tp/(tp+fp)

def recall(tp, fp, fn, tn):
  return tp/(tp+fn)

def F1(p, r):
  if p == 0 and r == 0:
    return 0
  else:
    return 2*p*r/(p+r)

series_type = 'publicity'
legislature = 54

outliers = []
with open('outlier-group.csv') as csvfile:
  spamreader = csv.reader(csvfile, delimiter=',', quotechar='"')
  next(spamreader, None)
  for row in spamreader:
    outliers.append(row[0])

for method in ['JS', 'cosine', 'robust']:
  for k in [2,3,4,5]:
    with open('../data/{}/dump/{}/k-{}/dump-clusters-{}.json'.format(series_type, method, k, legislature)) as jsonfile:
      data = json.load(jsonfile)

      clusters = merge_min_clusters(data, 3, legislature)
      
      print('Running for method={}, k={}'.format(method, k))

      max_score = 0
      best_cluster = []
      for cluster in clusters:
        tp, fp, fn, tn = stats(cluster, outliers, [i for sub in clusters for i in sub])
        score = F1(precision(tp, fp, fn, tn),recall(tp, fp, fn, tn))
        if score > max_score:
          max_score = score
          best_cluster = cluster
      
      data = "Best cluster: {}".format(best_cluster)
      best_cluster_str = (data[:75] + '...') if len(data) > 75 else data
      print("Max score: {}".format(max_score))
      print(best_cluster_str)