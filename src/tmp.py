from profiler import Profiler
import matplotlib.pyplot as plt
import numpy as np
import json

def read_mstknn_dump(legislature):
  with open('../data/dump-clusters-'+str(legislature)+'.json') as jsonfile:    
    data = json.load(jsonfile)

  return data

def mstknn_analysis(legislatures):
  #for legislature in legislatures:
  legislature = 54
  profiler = Profiler()
  series = profiler.read_congressman_json(legislature)
  clusters = read_mstknn_dump(legislature)
  for cluster in clusters:
    avgs = []
    for i in range(48):
      avg = []
      if isinstance(cluster,list):
        for congressman_id in cluster:
          avg.append(series.get(congressman_id)[4][i])
      elif isinstance(cluster, str):
        congressman_id = cluster
        avg.append(series.get(congressman_id)[4][i])

      avgs.append(np.average(avg))

    print(cluster)
    print(series.get(congressman_id)[0])
    plt.plot(avgs)
    plt.show()

mstknn_analysis(None)