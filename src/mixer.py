import util
import json
import os

mixed_data = dict()
with open('../data/JSON/norm/congressman_telecommunication_ts.json') as jsonfile:
  base_file = json.load(jsonfile) 

pi = 0
for name in base_file.keys():
  pi += 1
  util.printProgressBar(pi, len(base_file.keys()), prefix='Mixing data', suffix='Complete')
  if name not in mixed_data.keys():
    mixed_data[name] = list()

  for idx in range(len(base_file[name][4])):
    for file in os.listdir('../data/JSON/norm/'):
      if file != 'congressman_ts.json':

        with open('../data/JSON/norm/{}'.format(file)) as jsonfile:
          congressman_data = json.load(jsonfile)

      mixed_data[name].append(congressman_data[name][4][idx])

for name in mixed_data.keys():
  base_file[name][4] = mixed_data[name]

with open('../data/JSON/norm/congressman_mixed_ts.json','w') as jsonfile:
  json.dump(base_file, jsonfile)