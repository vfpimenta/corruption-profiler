from profiler import Profiler
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns
import random
import json
import os

import kde_joyplot

def get_date_range(legislature):
  datelist = pd.date_range(start="2009-04", freq='M', end="2016-09")
  return {
    53: datelist[0:22],
    54: datelist[22:70],
    55: datelist[70:89]
  }[legislature]

def read_mstknn_dump(legislature, k, method='JS'):
  with open('../data/dump/{}/k-{}/dump-clusters-{}.json'.format(method, k, legislature)) as jsonfile:    
    data = json.load(jsonfile)

  return data

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

  print('Merged clusters into min size {}'.format(min_value))
  print('The final clusters can be fount on {}'.format(filepath))
  return merged

def evaluate_avg(cluster, series):
  avgs = list()

  for i in range(len(next(iter(series.values()))[4])):
    avg = list()
    for congressman_id in cluster:
      avg.append(series.get(congressman_id)[4][i])

    avgs.append(np.average(avg))

  return avgs

def evaluate_dist(cluster, series):
  dist = list()

  for congressman_id in cluster:
    dist.append(np.average(series.get(congressman_id)[4]))

  return dist

def main(legislatures, k, func, method='JS', save=False):
  profiler = Profiler()
  for legislature in legislatures:
    series = profiler.read_congressman_json(legislature)
    clusters = merge_min_clusters(read_mstknn_dump(legislature, k, method), 3, legislature)
    cluster_idx = 0
    results = [list(), list()]
    for cluster in clusters:
      cluster_idx += 1
      #fig, ax = plt.subplots( nrows=1, ncols=1 )

      if func == 'avg':
        result = evaluate_avg(cluster, series)
        ax.plot(get_date_range(legislature), result)
      elif func == 'dist':
        result = evaluate_dist(cluster, series)
        if np.count_nonzero(result) == 0:
          result = (result + 0.00000001*np.random.randn(len(result))).tolist()
        #sns.distplot(result, ax=ax)

      for el in result:
        results[0].append(str(cluster_idx))
        results[1].append(el)

      if save:
        directory = '../img/graphs/{}/{}/k-{}/term-{}-groups/'.format(method, func, k, legislature)
        if not os.path.exists(directory):
          os.makedirs(directory)

        fig.savefig('../img/graphs/{}/{}/k-{}/term-{}-groups/region-graph-group{}.png'.format(method, func, k, legislature, cluster_idx), bbox_inches='tight')
        plt.close(fig)

    df = pd.DataFrame(results)
    df = df.transpose()
    df.columns = ['g', 'x']
    m = df.g.map(ord)
    kde_joyplot.plot(df)

if __name__ == '__main__':
  main([54],k=3,func='dist',method='robust')
  # for k in [3, 4, 5]:
  #   for method in ["robust","JS"]:
  #     for func in ["avg", "dist"]:
  #       print('[DEBUG] Running analysis for k={}, method={} and func={}'.format(k, method, func))
  #       main([53,54,55], k, func, method)