from profiler import Profiler
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns
import random
import json
import shutil
import os

import kde_joyplot

def get_date_range(legislature, section):
  datelist = pd.date_range(start="2009-04", freq='M', end="2016-09")
  return {
    53: datelist[0:22],
    54: datelist[22:70],
    55: datelist[70:89]
  }[legislature][section[0]:section[1]]

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

def evaluate_avg(cluster, series, section):
  avgs = list()

  for i in range(section[0], section[1]):
    avg = list()
    for congressman_id in cluster:
      avg.append(series.get(congressman_id)[4][i])

    avgs.append(np.average(avg))

  return avgs

def evaluate_dist(cluster, series, section):
  dist = list()

  for congressman_id in cluster:
    dist.append(np.average(series.get(congressman_id)[4][section[0]:section[1]]))

  return dist

def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in range(0, len(l), n):
        yield l[i:i + n]

def get_sections(legislature, series, split=None):
  expense_length = len(next(iter(series.values()))[4])
  idxs = range(expense_length)

  if legislature == 53 or legislature == 55:
    raise Exception('Not implemented yet!')
  else:
    if split == None:
      return [(0, expense_length)]
    elif split == 'quarterly':
      return [(chunk[0], chunk[-1]) for chunk in chunks(idxs, 3)]
    elif split == 'semiannual':
      return [(chunk[0], chunk[-1]) for chunk in chunks(idxs, 6)]
    elif split == 'annually':
      return [(chunk[0], chunk[-1]) for chunk in chunks(idxs, 12)]

def main(legislatures, k, func, method='JS', split=None, save=False):
  profiler = Profiler(light=True)
  for legislature in legislatures:
    series = profiler.read_congressman_json(legislature)
    clusters = merge_min_clusters(read_mstknn_dump(legislature, k, method), 3, legislature)
    cluster_idx = 0
    # results = [list(), list()]
    for cluster in clusters:
      cluster_idx += 1
      section_idx = 0
      for section in get_sections(legislature, series, split):
        section_idx += 1
        fig, ax = plt.subplots( nrows=1, ncols=1 )

        if func == 'avg':
          result = evaluate_avg(cluster, series, section)
          plt.xticks(rotation=70)
          ax.plot(get_date_range(legislature, section), result)
        elif func == 'dist':
          result = evaluate_dist(cluster, series, section)
          if np.count_nonzero(result) == 0:
            result = (result + 0.00000001*np.random.randn(len(result))).tolist()
          sns.distplot(result, norm_hist=True, ax=ax)

        # for el in result:
        #   results[0].append(str(cluster_idx))
        #   results[1].append(el)

        if save:
          directory = '../img/graphs/{}/{}/k-{}/term-{}-groups/section-{}'.format(method, func, k, legislature, section_idx)
          if not os.path.exists(directory):
            os.makedirs(directory)

          fig.savefig('../img/graphs/{}/{}/k-{}/term-{}-groups/section-{}/region-graph-group{}.png'.format(method, func, k, legislature, section_idx, cluster_idx), bbox_inches='tight')
          plt.close(fig)

    # df = pd.DataFrame(results)
    # df = df.transpose()
    # df.columns = ['g', 'x']
    # m = df.g.map(ord)
    # kde_joyplot.plot(df)

if __name__ == '__main__':
  shutil.rmtree('../img/graphs/')
  for k in [3]:
    for method in ["robust"]:
      for func in ["avg", "dist"]:
        print('[DEBUG] Running analysis for k={}, method={} and func={}'.format(k, method, func))
        main([54], k, func, method, split='semiannual', save=True)