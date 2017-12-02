from profiler import Profiler
import csv

max_subquota = 3

p = Profiler()
with open('../data/subquota.csv','r') as csvfile:
  reader = csv.reader(csvfile)
  idx = 0
  for row in reader:
    idx += 1
    p.read_congressman_json(54, subquota_description=row[0])
    if idx >= max_subquota:
      break