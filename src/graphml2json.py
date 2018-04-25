from networkx.readwrite import json_graph
from optparse import OptionParser
import networkx as nx
import json
import re

parser = OptionParser()
parser.add_option('-s', '--source', dest='source', type='str', help='Source file (graphml)', metavar='STRING')
parser.add_option('-t', '--target', dest='target', type='str', help='Target file (json)', metavar='STRING')

(options, args) = parser.parse_args()

G=nx.read_graphml(options.source, str)
data = json_graph.node_link_data(G)

for i in range(len(data['nodes'])):
	data['nodes'][i]['id'] = i

# for node in data['nodes']:
#   str=node['id']
#   node['id']=[int(s) for s in str.split("n") if s.isdigit()][0]

with open(options.target, 'w') as f:
  json.dump(data, f, indent=4)