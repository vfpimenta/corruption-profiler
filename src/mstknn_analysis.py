from profiler import Profiler
import matplotlib.pyplot as plt
import numpy as np
import json
import os

def read_mstknn_dump(legislature):
  with open('../data/dump-clusters-'+str(legislature)+'.json') as jsonfile:    
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

  filepath = '__pycache__/term-{}-cluster.out'.format(legislature)
  with open(filepath, 'w') as file:
    file.write(str(merged))

  print('Merged clusters into min size {}'.format(min_value))
  print('The final clusters can be fount on {}'.format(filepath))
  return merged

def evaluate_avg(cluster, series):
  avgs = list()

  for i in range(len(next(iter(series.values()))[4])):
    avg = list()
    if isinstance(cluster,list):
      for congressman_id in cluster:
        avg.append(series.get(congressman_id)[4][i])
    elif isinstance(cluster, str):
      congressman_id = cluster
      avg.append(series.get(congressman_id)[4][i])

    avgs.append(np.average(avg))

  return avgs

def main(legislatures):
  profiler = Profiler()
  for legislature in legislatures:
    series = profiler.read_congressman_json(legislature)
    clusters = merge_min_clusters(read_mstknn_dump(legislature), 3, legislature)
    cluster_idx = 0
    for cluster in clusters:
      cluster_idx += 1
      result = evaluate_avg(cluster, series)

      fig, ax = plt.subplots( nrows=1, ncols=1 )
      ax.plot(result)

      directory = '../img/graphs/JS/term-{}-groups/'.format(legislature)
      if not os.path.exists(directory):
        os.makedirs(directory)

      fig.savefig('../img/graphs/JS/term-{}-groups/region-graph-group{}.png'.format(legislature, cluster_idx), bbox_inches='tight')
      plt.close(fig)

if __name__ == '__main__':
  main([53, 54, 55])